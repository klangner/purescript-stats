module Test.Statistics.Sample.Histogram (testHistogram) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Tuple (Tuple(..))
import Test.Assert (assert, ASSERT)
import Statistics.Sample.Histogram as H


testHistogram :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testHistogram = do

    log "\n# Test Histogram"
    let s1 = [3.0, 1.0, 2.0, 2.0, 1.0, 2.0]

    log "* Histogram"
    log $ show $ H.histogram s1
    assert $ H.histogram s1 == [Tuple 2.0 3, Tuple 1.0 2, Tuple 3.0 1]
