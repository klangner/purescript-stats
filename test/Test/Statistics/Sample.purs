module Test.Statistics.Sample (testSample) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Test.Assert (assert, ASSERT)
import Math (sqrt)
import Statistics.Sample as S


testSample :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSample = do

    log "\n# Test Sample data structure"
    let s1 = [1.0, 2.0, 3.0]
    let s2 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let s3 = [1.0, 2.0, 3.0, 2.0, 1.0, 2.0]

    log "* mean"
    assert $ S.mean s1 == 2.0

    log "* variance"
    assert $ S.variance s2 == 35.0/12.0

    log "* standard deviation"
    assert $ S.stddev s2 == sqrt (35.0/12.0)

    log "* mode"
    log $ show (S.mode s3)
    assert $ S.mode s3 == Just 2.0
