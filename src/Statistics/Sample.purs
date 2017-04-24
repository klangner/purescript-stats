module Statistics.Sample 
  ( Sample
  , histogram
  , mean
  , mode
  , stddev
  , variance
  ) where

import Prelude 
import Data.Array as A
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Math (pow, sqrt)


-- | Sample data  
type Sample a = Array a


-- Calculate mean
mean :: Sample Number -> Number
mean xs = sum xs / toNumber (A.length xs)


-- Calculate variance
variance :: Sample Number -> Number
variance xs = sum (map (\v -> pow (v-mu) 2.0) xs) / toNumber (A.length xs)
  where 
    mu = mean xs


-- Calculate standard deviation
stddev :: Sample Number -> Number
stddev = sqrt <<< variance


-- Calculate mode
mode :: ∀ a. Ord a => Sample a -> Maybe a
mode xs = fst <$> A.foldl f Nothing xm
  where
    xm :: Array (Tuple a Int)
    xm = M.toUnfoldable (histogram xs)
    f :: Maybe (Tuple a Int) -> Tuple a Int -> Maybe (Tuple a Int)
    f Nothing x = Just x
    f (Just tu) x = if (snd x) > (snd tu) then Just x else Just tu


-- Calculate histogram
histogram :: ∀ a. Ord a => Sample a -> M.Map a Int
histogram xs = A.foldl f (M.empty :: M.Map a Int) xs
  where
    f m x = M.alter g x m
    g (Just y) = Just (y + 1)
    g Nothing = Just 1