module Test.Helper where 

import Prelude
import Math (abs)


class AlmostEq a where
  almostEq :: a -> a -> Boolean
infix 4 almostEq as ~=

instance almostEqNumber :: AlmostEq Number where
  almostEq x y = abs (x - y) < 1e-14

instance almostEqInt :: AlmostEq Int where
  almostEq x y = x == y

-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- Check that function is nondecreasing
-- If the points are very close to each other then don't check because of possible numeric errors
monotonicallyIncreases :: (Number -> Number) -> Number -> Number -> Boolean
monotonicallyIncreases f x1 x2 
  | abs (x1 - x2) < 0.1  = true
  | otherwise = f (min x1 x2) <= f (max x1 x2)

