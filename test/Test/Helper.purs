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
-- monotonicallyIncreases :: forall a b. Ord a b => (a -> b) -> a -> a -> Boolean
monotonicallyIncreases :: ∀ a b. Ord a => Ord b => (a -> b) -> a -> a -> Boolean
monotonicallyIncreases f x1 x2 = f (min x1 x2) <= f (max x1 x2)

-- Check that function is nondecreasing taking rounding errors into
-- account.
--
-- In fact funstion is allowed to decrease less than one ulp in order
-- to guard againist problems with excess precision. On x86 FPU works
-- with 80-bit numbers but doubles are 64-bit so rounding happens
-- whenever values are moved from registers to memory
-- monotonicallyIncreasesIEEE :: (Ord a, IEEE.IEEE b)  => (a -> b) -> a -> a -> Bool
-- monotonicallyIncreasesIEEE f x1 x2 =
--   y1 <= y2 || (y1 - y2) < y2 * IEEE.epsilon
--   where
--     y1 = f (min x1 x2)
-- y2 = f (max x1 x2)