{-# LANGUAGE StandaloneDeriving #-}
module Data.Text.Index
  (
    Index
  
  , lookupWord
  , lookupPhrase
  , Weight, toDouble
  , size
  
  , empty
  , addDocument
  , removeDocument
  )where

import qualified Data.Search.Results as Results

import           Data.Foldable   (foldMap, for_)
import           Data.Functor    ((<$>))
import           Data.List       (foldl', sortOn, intercalate)
import           Data.Monoid     ((<>), Endo(Endo, appEndo))
import           Data.Ord        (Down(Down))
import qualified Data.OrdPSQ             as PSQ
import qualified Data.Text               as Text
import           Data.Text       (Text)
import qualified Data.Text.ICU           as ICU
import qualified Data.Text.ICU.Char      as ICU
import qualified Data.Text.ICU.Normalize as ICU
import qualified Data.TST                as TST
import           Data.TST        (TST)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           Prelude hiding (lookup, words, Word)


-- TODO: exact matches within long strings are not found right now (unless
-- the start of the match happens to coincide with a chunk boundary).

-- The index contains many (trimmed-down) variants of all source words.
type Index id
  = TST Char (Entries id)

-- For a given variant (this is a leaf of the TST), we have a set
-- of original words, together with the source id, and its weight (indicating
-- how much the original word was trimmed).
type Entries id
  = PSQ.OrdPSQ id Weight Word

-- A 'Word' does not contain spaces.
type Word
  = Text

newtype Weight
  = Weight Double
  deriving (Eq, Show, Typeable)

toDouble :: Weight -> Double
toDouble (Weight d) = d

instance Monoid Weight where
  mappend (Weight a) (Weight b) = Weight $ a + b
  mempty = Weight 0

reweigh :: (Double -> Double) -> Weight -> Weight
reweigh f (Weight d) = Weight (f d)

instance Ord Weight where
  compare (Weight a) (Weight b) = compare a b

-- Lookup.

lookupExact :: (Ord id) =>
  Index id -> Word -> Results.T id Weight Word
lookupExact index t = case TST.lookup (realChars t) index of
  Just entries -> Results.fromPSQ entries
  Nothing      -> Results.empty

lookupWord :: (Ord id) =>
  Index id -> Word -> Results.T id Weight Word
lookupWord index t = Results.union $ map f variations
 where
  f (t', w) = Results.changeWeight (w <>) $
    lookupExact index t'
  variations = deletions defaultWeights =<< chop (normalise t)

lookupPhrase :: (Ord id) => Index id -> Text -> Results.T id Weight Word
lookupPhrase index t = case words t of
  -- Empty search term gives no results. Do we want a way
  -- to return all results?
  []    -> Results.empty
  terms -> Results.intersection $ map (lookupWord index) terms

size :: Index id -> Int
size = length . TST.toList

-- Creation.

empty :: Index id
empty = TST.empty

addVariant :: (Ord id) => id -> Word -> (Word, Weight) -> Index id -> Index id
addVariant i original (variant, w) = TST.insertWith
  (const $ snd . PSQ.alter f i)
  (realChars variant)
  (PSQ.singleton i w original)
 where
  f (Just (w', original'))
    | w' <= w = ((), Just (w', original'))
  f _         = ((), Just (w , original ))

addWord :: (Ord id) => id -> Word -> Index id -> Index id
addWord i t = appEndo . foldMap (Endo . addVariant i t) $
  vary defaultWeights t

addDocument :: (Ord id) => id -> Text -> Index id -> Index id
addDocument i = appEndo . foldMap (Endo . addWord i) . words

removeDocument :: (Ord id) => id -> Index id -> Index id
removeDocument i = fmap (PSQ.delete i)

words :: Text -> [Text]
words = map ICU.brkBreak .
  filter ((/=) ICU.Uncategorized . ICU.brkStatus) .
  ICU.breaks (ICU.breakWord ICU.Current)

normalise :: Text -> Text
normalise = ICU.normalize ICU.NFKD . Text.toCaseFold

weightedLength :: Weights -> Text -> Weight
weightedLength wts = mconcat . map (character wts) . Text.unpack

-- Generate all variants of a word.
-- Some of these contain holes, others are just shortened versions.
vary :: Weights -> Word -> [(Word, Weight)]
vary wts t = shorten wts =<< deletions wts =<< chop =<< [normalise t]

shorten :: Weights -> (Word, Weight) -> [(Word, Weight)]
shorten wts (t, w)
  -- Non-trivial deletion variants of a word are not further shortened.
  | w > Weight 0.9  = [(t, w)]
  | otherwise       = (t, w) : concatMap
      (\ br -> weigh (ICU.brkPrefix br) ++ weigh (ICU.brkSuffix br))
      breaks
  where
    -- Break up a string in characters.
    breaks = ICU.breaks (ICU.breakCharacter ICU.Current) t
    weigh st
      | Text.null st = []
      | otherwise    = [(st, w <> _N <> n)]
     where
      n  = reweigh negate $ weightedLength wts st
    _N = weightedLength wts t

deletions :: Weights -> (Word, Weight) -> [(Word, Weight)]
deletions wts (t, w) = step (t, w) where
  maximalWeight = budget wts $ weightedLength wts t
  step :: (Word, Weight) -> [(Word, Weight)]
  step (t, w) = go t w 1 where
    go t w start
      | w > maximalWeight = []
      | Text.null t       = [(t, w)]
      | otherwise         = (t, w) : concatMap
          (delete w)
          [(i, Text.splitAt i t) | i <- [start .. Text.length t]]
    delete w (i, (leftC, right)) = go
      (Text.append left (Text.cons hole right))
      (w <> cost)
      (succ i)
     where
      c = Text.last leftC
      left = Text.init leftC
      cost = reweigh (continuationCost *) characterCost
      continuationCost
        | not (Text.null left) && Text.last left == hole = continuation wts
        | otherwise                                      = 1
      characterCost = character wts c

data Weights
  = Weights
    { continuation :: Double
    , character    :: Char -> Weight
    , budget       :: Weight -> Weight
    }

defaultWeights :: Weights
defaultWeights = Weights
  { continuation = 0.75
  , character    = \ c -> case ICU.property ICU.GeneralCategory c of
      ICU.NonSpacingMark       -> Weight 0.2
      ICU.EnclosingMark        -> Weight 0.2
      ICU.CombiningSpacingMark -> Weight 0.2
      ICU.OtherPunctuation     -> Weight 0.4
      _                        -> Weight 1
  , budget = reweigh $ min 2 . \ l -> 0.5 + l / 5
  }

hole :: Char
hole = '\xFFFC'

showHoles :: Text -> Text
showHoles = Text.map (\ c -> if c == hole then '□' else c)

realChars :: Word -> [Char]
realChars = filter (/= hole) . Text.unpack

-- Dealing with long words.

-- Chop up a word in overlapping chunks, of maximal length 15
-- and typical length 10.
chop :: Word -> [(Word, Weight)]
chop t
  | Text.length t <= maximalChunk = [(t, Weight 0)]
  | otherwise                     = (Text.take maximalChunk t, Weight 0) :
      (map (flip (,) $ Weight 1) . chop' $ Text.drop overlap t)
  where
    chop' t
      | Text.length t <= maximalChunk = [t]
      | otherwise                     = Text.take typicalChunk t : chop' (Text.drop overlap t)
    maximalChunk = 3 * overlap
    typicalChunk = 2 * overlap
    overlap      = 5

-- Helper functions and tests.

printVariations :: [(Word, Weight)] -> IO ()
printVariations = mapM_ $ \ (t, w) -> do
  putStr (Text.unpack . showHoles $ t)
  putStrLn (" " ++ show w)

testDocument :: Text
testDocument = Text.pack "Dit is een hele zin, met allemaal woorden erin. Ook woorden met accent, als café. Daarnaast wat cijfers: 0 123 4.567. En natuurlijk symbolen ☺☹!"

dr :: Text
dr = Text.pack "driehoeksongelijkheid"
