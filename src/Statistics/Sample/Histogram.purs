module Statistics.Sample.Histogram
  ( Histogram
  , histogram
  ) where

import Prelude 
import Data.Array as A
import Data.Map as M
import Data.Maybe(Maybe(..))
import Data.Tuple (Tuple)


-- | Sample data  
type Histogram a = Array (Tuple a Int)


-- | Calculate histogram. Create as many bins as necessary
histogram :: ∀ a. Ord a => Array a -> Histogram a --M.Map a Int
histogram xs = M.toUnfoldable $ A.foldl f (M.empty :: M.Map a Int) xs
  where
    f m x = M.alter g x m
    g (Just y) = Just (y + 1)
    g Nothing = Just 1
