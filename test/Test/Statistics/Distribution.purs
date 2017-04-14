module Test.Statistics.Distribution (testDistribution) where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary
                       , QC
                       , arbitrary
                       , quickCheck
                       , (<?>))
import Test.QuickCheck.Gen (choose)
import Statistics.Distribution (class ContDistr, density)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)


-- Helper type to define Arbitrary instances. Without this type they will be orphaned
-- And this file won't compile
newtype D a = D a 

instance arbNormal :: Arbitrary (D NormalDistribution) where
    arbitrary = do
        mu <- arbitrary
        std <- choose 1e-3 1e6
        pure $ D $ normalDistr mu std


-- Main test method
testDistribution :: ∀ eff. QC eff Unit
testDistribution = do
    log "\n# Distribution tests"

    log "## Normal distribution"
    normalDistrTests 


-- Test normal distributions
normalDistrTests :: ∀ eff. QC eff Unit
normalDistrTests= do
    log "* PDF sanity"
    quickCheck $ \(D d :: D NormalDistribution) x -> 
        pdfSanityCheck d x <?> "Distribution: " <> show d <> ", x: " <> show x


-- PDF is always positive
pdfSanityCheck :: ∀ t. ContDistr t => t -> Number -> Boolean
pdfSanityCheck d x = (density d x) >= 0.0
