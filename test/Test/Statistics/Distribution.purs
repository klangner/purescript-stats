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
import Statistics.Distribution.Uniform (UniformDistribution, uniformDistr)


-- Helper type to define Arbitrary instances. Without this type they will be orphaned
-- And this file won't compile
newtype D a = D a 

instance arbNormal :: Arbitrary (D NormalDistribution) where
    arbitrary = do
        mu <- arbitrary
        std <- choose 1e-3 1e6
        pure $ D $ normalDistr mu std

instance arbUniform :: Arbitrary (D UniformDistribution) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        let b' = if a == b then a + 1.0 else b
        pure $ D $ uniformDistr a b'


-- Main test method
testDistribution :: ∀ eff. QC eff Unit
testDistribution = do
    
    log "\n# Distribution tests"

    log "## Normal distribution"
    normalDistrTests 

    log "## Uniform distribution"
    uniformDistrTests 


-- Test normal distributions
normalDistrTests :: ∀ eff. QC eff Unit
normalDistrTests= do
    
    log "* PDF sanity"
    quickCheck $ \(D d :: D NormalDistribution) x -> 
        pdfSanityCheck d x <?> "Distribution: " <> show d <> ", x: " <> show x


-- Test uniform distributions
uniformDistrTests :: ∀ eff. QC eff Unit
uniformDistrTests= do
    
    log "* PDF sanity"
    quickCheck $ \(D d :: D UniformDistribution) x -> 
        pdfSanityCheck d x <?> "Distribution: " <> show d <> ", x: " <> show x
    

-- PDF is always positive
pdfSanityCheck :: ∀ t. ContDistr t => t -> Number -> Boolean
pdfSanityCheck d x = (density d x) >= 0.0
