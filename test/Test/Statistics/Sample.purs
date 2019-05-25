module Test.Statistics.Sample (testSample) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Math (sqrt)
import Statistics.Sample as S
import Test.Assert (assert)


testSample :: Effect Unit
testSample = do

    log "\n# Test Sample data structure"
    let s1 = [1.0, 2.0, 3.0]
    let s2 = [1.0, 2.0, 5.0, 6.0, 3.0, 4.0]
    let s3 = [3.0, 1.0, 2.0, 2.0, 1.0, 2.0]
    let s4 = [3.0, 1.0, 2.0, 5.8, 2.0, 1.0, 2.0]

    log "* mean"
    assert $ S.mean s1 == 2.0

    log "* max"
    assert $ S.max s2 == Just 6.0

    log "* min"
    assert $ S.min s3 == Just 1.0

    log "* median odd"
    assert $ S.median s4 == 2.0

    log "* median even"
    assert $ S.median s2 == 4.5

    log "* variance"
    assert $ S.variance s2 == 35.0/12.0

    log "* standard deviation"
    assert $ S.stddev s2 == sqrt (35.0/12.0)

    log "* mode"
    assert $ S.mode s3 == Just 2.0
