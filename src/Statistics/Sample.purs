module Statistics.Sample 
  ( Sample
  , histogram
  , max
  , mean
  , median
  , min
  , mode
  , stddev
  , variance
  ) where

import Prelude 
import Data.Array as A
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
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
    f (Just tu) x = Just $ if (snd x) > (snd tu) then x else tu


-- Calculate histogram
histogram :: ∀ a. Ord a => Sample a -> M.Map a Int
histogram xs = A.foldl f (M.empty :: M.Map a Int) xs
  where
    f m x = M.alter g x m
    g (Just y) = Just (y + 1)
    g Nothing = Just 1


-- | Maximum value in the sample
max :: ∀ a. Ord a => Sample a -> Maybe a
max xs = A.foldl f Nothing xs
  where 
    f (Just m) x = Just $ if x > m then x else m
    f Nothing x = Just x


-- | Minimal value in the sample
min :: ∀ a. Ord a => Sample a -> Maybe a
min xs = A.foldl f Nothing xs
  where 
    f (Just m) x = Just $ if x < m then x else m
    f Nothing x = Just x    


median :: Sample Number -> Number    
median [] = 0.0
median xs = if mod n 2 == 0 then ((at half) + (at (half+1))) / 2.0 else at half
  where
    sorted = A.sort xs
    n = A.length xs
    half = n / 2
    at i = fromMaybe 0.0 $ A.index xs i