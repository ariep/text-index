module Data.Search.Results
  ( T
  , size
  , take
  , toList
  , filterM
  , union
  , intersection
  , empty
  , fromPSQ
  , fromList
  , changeWeight
  ) where

import qualified Control.Monad as Monad
import qualified Data.List     as List
import           Data.Maybe  (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.OrdPSQ             as PSQ

import Prelude hiding (take)

data T id w v
  = PSQ (PSQ.OrdPSQ id w v)
  | List [(id, w, v)]

empty = fromList []

fromPSQ :: PSQ.OrdPSQ id w v -> T id w v
fromPSQ = PSQ

fromList :: [(id, w, v)] -> T id w v
fromList = List

ensurePSQ :: (Ord id, Ord w) => T id w v -> T id w v
ensurePSQ r@(PSQ _) = r
ensurePSQ   (List xs) = fromPSQ $ PSQ.fromList xs

instance (Show id, Show w, Show v) => Show (T id w v) where
  show r = "Results set of " ++ show (size r) ++ " results:\n" ++
    List.intercalate "\n" (map show $ toList r)

size :: T id w v -> Int
size (PSQ p)   = PSQ.size p
size (List xs) = List.length xs

take :: (Ord id, Ord w, Monoid w) => Int -> T id w v -> [(id, w, v)]
take n (PSQ p)   = PSQ.takeMin n p
take n (List xs) = List.take n xs

toList :: T id w v -> [(id, w, v)]
toList (PSQ p)   = PSQ.toList p
toList (List xs) = xs

filterM :: (Monad m) => ((id, v) -> m Bool) -> T id w v -> m (T id w v)
filterM f = fmap List . Monad.filterM (\ (i, _, v) -> f (i, v)) . toList

union :: (Ord id, Ord w, Monoid w) => [T id w v] -> T id w v
union rs = List.foldl' u empty sorted
 where
  sorted = List.sortOn size rs
  u a b@(List _)  = u a (ensurePSQ b)
  u a   (PSQ psq) = fromPSQ $ List.foldl'
    (\ psq (k, w, v) -> PSQ.insertWith combine k w v psq)
    psq
    (toList a)
  -- Used when the same source is contained in both results.
  combine _ (w, v) (w', _v') = (w `min` w', v)

intersection :: (Ord id, Ord w, Monoid w) => [T id w v] -> T id w v
intersection rs = List.foldl1' i sorted
 where
  sorted = List.sortOn size $ rs
  i a b@(List _)  = i a (ensurePSQ b)
  i a   (PSQ psq) = fromList . mapMaybe f . toList $ a where
    f (k, w, v) = case PSQ.lookup k psq of
      Nothing       -> Nothing
      Just (w', _v') -> Just (k, w <> w', v)

changeWeight :: (w -> w') -> T id w v -> T id w' v
changeWeight cw = fromList . map f . toList where
  f (k, w, v) = (k, cw w, v)
