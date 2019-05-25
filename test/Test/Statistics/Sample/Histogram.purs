module Test.Statistics.Sample.Histogram (testHistogram) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Statistics.Sample.Histogram as H
import Test.Assert (assert)


testHistogram :: Effect Unit
testHistogram = do

    log "\n# Test Histogram"
    let s1 = [3.0, 1.0, 2.0, 2.0, 1.0, 2.0]

    log "* Histogram"
    assert $ H.histogram s1 == [(Tuple 1.0 2), (Tuple 2.0 3), (Tuple 3.0 1)]
