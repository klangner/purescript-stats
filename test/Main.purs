module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Test.Assert (ASSERT)

import Test.Numeric.SpecFunctions (testSpecFun)
import Test.Statistics.Sample (testSample)
import Test.Statistics.Distribution (testDistribution)


main :: ∀ eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION, assert :: ASSERT | eff) Unit
main = do
  testSpecFun
  testSample
  testDistribution
