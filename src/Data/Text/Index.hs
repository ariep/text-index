{-# LANGUAGE StandaloneDeriving #-}
module Data.Text.Index where


import           Data.Foldable   (foldMap,for_)
import           Data.Functor    ((<$>))
import           Data.List       (foldl')
import           Data.Monoid     (Endo(Endo,appEndo))
import           Data.Ord        (Down(Down))
import qualified Data.OrdPSQ             as PSQ
import qualified Data.Text               as Text
import           Data.Text       (Text)
import qualified Data.Text.ICU           as ICU
import qualified Data.Text.ICU.Char      as ICU
import qualified Data.Text.ICU.Normalize as ICU
import qualified Data.TST                as TST
import           Data.TST        (TST)
import           GHC.Generics    (Generic)
import           Prelude hiding (lookup,words)


-- Types.

type Index id
  = TST Char (Entries id)

type Entries id
  = PSQ.OrdPSQ id (Down Weight) Word

type Word
  = Text

type Weight
  = Double

-- Lookup.

lookupExact :: (Ord id) => Int -> Word -> Index id -> [(id,Down Weight,Word)]
lookupExact n t index = case TST.lookup (realChars t) index of
  Just entries -> PSQ.takeMin n entries
  Nothing      -> []

lookup :: (Ord id) => Int -> Word -> Index id -> [(id,Weight,Word)]
lookup n t index = map (\ (i,Down w,t') -> (i,w,t')) .
  PSQ.takeMin n . psqUnions $ map f variations
 where
  f (t',w) = PSQ.fromList $ map (\ (i,Down w',t'') -> (i,Down $ w + w',t'')) $ lookupExact n t' index
  variations = deletions defaultWeights (normalise t,0)

psqUnions :: (Ord k,Ord p) => [PSQ.OrdPSQ k p v] -> PSQ.OrdPSQ k p v
psqUnions = foldl' psqUnion PSQ.empty where
  psqUnion big small = foldl'
    (\ psq (k,p,v) -> PSQ.insertWith combine k p v psq)
    big
    $ PSQ.toList small
  combine _ (p₁,v₁) (p₂,v₂)
    | p₁ <= p₂  = (p₁,v₁)
    | otherwise = (p₂,v₂)

-- Creation.

empty :: Index id
empty = TST.empty

addVariant :: (Ord id) => id -> Word -> (Word,Weight) -> Index id -> Index id
addVariant i original (variant,w) = TST.insertWith
  (const $ PSQ.insert i (Down w) original)
  (realChars variant)
  (PSQ.singleton i (Down w) original)

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
weightedLength wts = sum . map (character wts) . Text.unpack

vary :: Weights -> Word -> [(Word,Weight)]
vary wts t = shorten wts =<< deletions wts =<< chop =<< [normalise t]

shorten :: Weights -> (Word,Weight) -> [(Word,Weight)]
shorten wts (t,w)
  | w < -0.9  = [(t,w)]
  | otherwise = (t,w) : concatMap
      (\ br -> weigh (ICU.brkPrefix br) ++ weigh (ICU.brkSuffix br))
      breaks
  where
    breaks = ICU.breaks (ICU.breakCharacter ICU.Current) t
    weigh st
      | Text.null st = []
      | otherwise    = [(st,w - _N + weightedLength wts st)]
    _N = weightedLength wts t

deletions :: Weights -> (Word,Weight) -> [(Word,Weight)]
deletions wts (t,w) = step (t,w) where
  minimalWeight = negate $ budget wts $ weightedLength wts t
  step :: (Word,Weight) -> [(Word,Weight)]
  step (t,w) = go t w 1 where
    go t w start
      | w < minimalWeight = []
      | Text.null t       = [(t,w)]
      | otherwise         = (t,w) : concatMap
          (delete w)
          [(i,Text.splitAt i t) | i <- [start .. Text.length t]]
    delete w (i,(leftC,right)) = go
      (Text.append left (Text.cons hole right))
      (w - cost)
      (succ i)
     where
      c = Text.last leftC
      left = Text.init leftC
      cost = continuationCost * characterCost
      continuationCost
        | not (Text.null left) && Text.last left == hole = continuation wts
        | otherwise                                      = 1
      characterCost = character wts c

data Weights
  = Weights
    { continuation :: Weight
    , character    :: Char -> Weight
    , budget       :: Weight -> Weight
    }

defaultWeights :: Weights
defaultWeights = Weights
  { continuation = 0.75
  , character    = \ c -> case ICU.property ICU.GeneralCategory c of
      ICU.NonSpacingMark       -> 0.2
      ICU.EnclosingMark        -> 0.2
      ICU.CombiningSpacingMark -> 0.2
      ICU.OtherPunctuation     -> 0.4
      _                        -> 1
  , budget = min 2 . \ l -> 0.5 + l / 5
  }

hole :: Char
hole = '\xFFFC'

showHoles :: Text -> Text
showHoles = Text.map (\ c -> if c == hole then '□' else c)

realChars :: Word -> [Char]
realChars = filter (/= hole) . Text.unpack

-- Dealing with long words.

chop :: Word -> [(Word,Weight)]
chop t
  | Text.length t < maximalChunk = [(t,0)]
  | otherwise                    = map (flip (,) $ -1) $ chop' t
  where
    chop' t
      | Text.length t < maximalChunk = [t]
      | otherwise                    = Text.take typicalChunk t : chop' (Text.drop overlap t)
    maximalChunk = 3 * overlap
    typicalChunk = 2 * overlap
    overlap      = 5

-- Helper functions and tests.

printVariations :: [(Word,Weight)] -> IO ()
printVariations = mapM_ $ \ (t,w) -> do
  putStr (Text.unpack . showHoles $ t)
  putStrLn (" " ++ show w)

testDocument :: Text
testDocument = Text.pack "Dit is een hele zin, met allemaal woorden erin. Ook woorden met accent, als café. Daarnaast wat cijfers: 0 123 4.567. En natuurlijk symbolen ☺☹!"

dr :: Text
dr = Text.pack "driehoeksongelijkheid"
