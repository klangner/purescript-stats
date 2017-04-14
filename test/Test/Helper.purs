module Test.Helper where 

import Prelude
import Math (abs)


class AlmostEq a where
  almostEq :: a -> a -> Boolean
infix 4 almostEq as ~=

instance almostEqNumber :: AlmostEq Number where
  almostEq x y = abs (x - y) < 0.000001

instance almostEqInt :: AlmostEq Int where
almostEq x y = x == y

-- | Phantom typed value used to select right instance in QC tests
data T a = T