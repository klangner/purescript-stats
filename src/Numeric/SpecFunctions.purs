module Numeric.SpecFunctions where 

import Prelude
import Math (abs, exp)


-- | fractional error in math formula less than 1.2 * 10 ^ -7.
-- | although subject to catastrophic cancellation when z in very close to 0
-- | from Chebyshev fitting formula for erf(z) from Numerical Recipes, 6.2
-- | Implements the Gauss error function.
-- | 
-- |             erf(z) = 2 / sqrt(pi) * integral(exp(-t*t), t = 0..z) 
-- | 
-- | Taken from: http://introcs.cs.princeton.edu/java/21function/ErrorFunction.java.html
erf :: Number -> Number
erf x = if x >= 0.0 then ans else -ans
    where
        t = 1.0 / (1.0 + 0.5 * (abs x))
        -- use Horner's method
        ans = 1.0 - t * exp ( -x*x - 1.26551223 +
                                            t * ( 1.00002368 +
                                            t * ( 0.37409196 + 
                                            t * ( 0.09678418 + 
                                            t * (-0.18628806 + 
                                            t * ( 0.27886807 + 
                                            t * (-1.13520398 + 
                                            t * ( 1.48851587 + 
                                            t * (-0.82215223 + 
                                            t * ( 0.17087277))))))))))

-- | erf complementary
erfc :: Number -> Number
erfc x = 1.0 - (erf x)