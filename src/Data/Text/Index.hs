{-# LANGUAGE StandaloneDeriving #-}
module Data.Text.Index
  ( Index

  , lookupWord
  , lookupPhrase
  , Weight, toDouble
  , size

  , empty
  , addDocument
  , addDocumentParts
  , removeDocument
  ) where

import qualified Data.Search.Results as Results

import qualified Data.Char.Properties.GeneralCategory as CharProp
import           Data.Foldable   (foldMap, for_)
import           Data.Functor    ((<$>))
import           Data.List       (foldl', sortOn, intercalate)
import           Data.Monoid     ((<>), Endo(Endo, appEndo))
import           Data.Ord        (Down(Down))
import qualified Data.OrdPSQ             as PSQ
import qualified Data.Text               as Text
import           Data.Text       (Text)
import qualified Data.TST                as TST
import           Data.TST        (TST)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           Prelude hiding (lookup, words, Word)
import qualified Prose.Segmentation.Words as Words
import qualified Prose.Normalization.Text as Normalise


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
  variations = deletions defaultWeights =<< chop (compose t)

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

addDocumentParts :: (Ord id) => id -> [Text] -> Index id -> Index id
addDocumentParts i = appEndo . foldMap (Endo . addDocument i)

addDocument :: (Ord id) => id -> Text -> Index id -> Index id
addDocument i = appEndo . foldMap (Endo . addWord i) . words

removeDocument :: (Ord id) => id -> Index id -> Index id
removeDocument i = fmap (PSQ.delete i)

words :: Text -> [Text]
words = Words.segmentT

compose, decompose :: Text -> Text
compose = Normalise.composeC
decompose = Normalise.decomposeKD

weightedLength :: Weights -> Text -> Weight
weightedLength wts = mconcat . map (character wts) . Text.unpack

-- Generate all variants of a word.
-- Some of these contain holes, others are just shortened versions.
vary :: Weights -> Word -> [(Word, Weight)]
vary wts t = shorten wts . first compose =<< deletions wts =<< chop =<< [decompose t]
 where
  first f (a, b) = (f a, b)

shorten :: Weights -> (Word, Weight) -> [(Word, Weight)]
shorten wts (t, w)
  -- Non-trivial deletion variants of a word are not further shortened.
  | w > Weight 0.9  = [(t, w)]
  | otherwise       = (t, w) : concatMap weigh substrings
  where
    -- Break up a string in characters.
    substrings = splitEverywhere t
    weigh st
      | Text.null st = []
      | otherwise    = [(st, w <> _N <> n)]
     where
      n  = reweigh negate $ weightedLength wts st
    _N = weightedLength wts t

splitEverywhere :: Text -> [Text]
splitEverywhere t = drop 1 (init $ Text.inits t) ++ drop 1 (init $ Text.tails t)

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
  , character    = \ c -> case CharProp.gcMajorClass $ CharProp.getGeneralCategory c of
      CharProp.ClLetter	     -> Weight 1
      CharProp.ClMark        -> Weight 0.2
      CharProp.ClNumber	     -> Weight 1
      CharProp.ClSeparator   -> Weight 0.2
      CharProp.ClPunctuation -> Weight 0.4
      CharProp.ClSymbol      -> Weight 1
      CharProp.ClOther       -> Weight 1
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
-- Words that do not need to be chopped have zero weight. Words that do get weight 1.
-- This enables us to store less variations of long words in the index.
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

to :: Text
to = Text.pack "telefoonoplader"
