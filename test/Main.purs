module Test.Main where

import Prelude

import Effect (Effect)
import Test.Numeric.SpecFunctions (testSpecFun)
import Test.Statistics.Distribution (testDistribution)
import Test.Statistics.Sample (testSample)
import Test.Statistics.Sample.Histogram (testHistogram)


main :: Effect Unit
main = do
  testSpecFun
  testSample
  testHistogram
  testDistribution
