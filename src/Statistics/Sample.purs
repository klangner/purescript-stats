module Statistics.Sample 
  ( Sample
  , mean
  , stddev
  , variance
  ) where

import Prelude 
import Data.Array as A
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
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
mode :: ∀ a. Eq a => Sample a -> Maybe a
mode xs = A.head xs