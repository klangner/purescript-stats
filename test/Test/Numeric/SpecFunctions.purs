module Test.Numeric.SpecFunctions (testSpecFun) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Numeric.SpecFunctions (erf)
import Test.Helper ((~=))
import Test.Assert (assert)


testSpecFun :: Effect Unit
testSpecFun = do
    log "\n# Test erf"
    log " * Test erf 1.0"
    assert $ erf 1.0 == 0.8427007877600067         -- actual = 0.84270079294971486934
    log " * Test erf -1.0"
    assert $ erf (-1.0) ~= -0.8427007877600068
    log " * Test erf 3.0"
    assert $ erf 3.0 == 0.9999779095015785         -- actual = 0.99997790950300141456
    log " * Test erf 30.0"
    assert $ erf 30.0 == 1.0
    log " * Test erf -30.0"
    assert $ erf (-30.0) == -1.0
    log " * Test erf 1.0E-20"
    assert $ erf 1.0E-20 == -3.0000000483809686E-8 -- actual = 1.13E-20
