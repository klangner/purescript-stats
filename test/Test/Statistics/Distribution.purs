module Test.Statistics.Distribution (testDistribution) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Test.QuickCheck (class Arbitrary
                       , arbitrary
                       , quickCheck
                       , (<?>))
import Test.QuickCheck.Gen (choose)
import Statistics.Distribution (density)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)


newtype D a = D a 

instance arbNormal :: Arbitrary (D NormalDistribution) where
    arbitrary = do
        mu <- arbitrary
        std <- choose 0.1 1000000.0
        pure $ D $ normalDistr mu std


testDistribution :: ∀ eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
testDistribution = do

    log "Normal distribution"
    contDistrTests --(T :: T NormalDistribution )


-- Tests for continuous distributions
contDistrTests :: ∀ eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
contDistrTests = do
    log "PDF sanity"
    quickCheck $ \(D d) x -> 
        pdfSanityCheck d x <?> "Distribution: " <> show d <> ", x: " <> show x


-- PDF is positive
pdfSanityCheck :: NormalDistribution -> Number -> Boolean
pdfSanityCheck d x = (density d x) >= 0.0



--   , testProperty "Quantile is CDF inverse" $ quantileIsInvCDF   t
--   , testProperty "quantile fails p<0||p>1" $ quantileShouldFail t
--   , testProperty "log density check"       $ logDensityCheck    t
--   , testProperty "complQuantile"           $ complQuantileCheck t


-- Tests for distributions which have CDF
-- cdfTests :: (Param d, Distribution d, QC.Arbitrary d, Show d) => T d -> [Test]
-- cdfTests t =
--   [ testProperty "C.D.F. sanity"        $ cdfSanityCheck         t
--   , testProperty "CDF limit at +inf"    $ cdfLimitAtPosInfinity  t
--   , testProperty "CDF limit at -inf"    $ cdfLimitAtNegInfinity  t
--   , testProperty "CDF at +inf = 1"      $ cdfAtPosInfinity       t
--   , testProperty "CDF at -inf = 1"      $ cdfAtNegInfinity       t
--   , testProperty "CDF is nondecreasing" $ cdfIsNondecreasing     t
--   , testProperty "1-CDF is correct"     $ cdfComplementIsCorrect t
-- ]