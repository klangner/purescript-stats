module Test.Statistics.Distribution (testDistribution) where

import Prelude
import Global (infinity)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary
                       , QC
                       , arbitrary
                       , quickCheck
                       , (<?>)
                       )
import Test.QuickCheck.Gen (choose)
import Test.Helper (monotonicallyIncreases, (~=))
import Statistics.Distribution ( class Distribution
                               , class ContDistr
                               , complCumulative
                               , cumulative
                               , density
                               )
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)
import Statistics.Distribution.Uniform (UniformDistribution, uniformDistr)


-- Helper type to define Arbitrary instances. Without this type they will be orphaned
-- And this file won't compile
newtype D a = D a 

-- Main test method
testDistribution :: ∀ eff. QC eff Unit
testDistribution = do
    
    log "\n# Distribution tests"

    log "\nNormal distribution"
    normalDistrTests 

    log "\nUniform distribution"
    uniformDistrTests 


-- ------------------------------------------------------------------------------------------------
-- Test distributions
-- In Haskell this is simpler. Due to the better Type system test could be groupped 
-- according to the class
-- Here each distribution checks the same properties
-- ------------------------------------------------------------------------------------------------

-- Normal distributions
normalDistrTests :: ∀ eff. QC eff Unit
normalDistrTests= do
    
    log "* CDF sanity"
    quickCheck $ \(D d :: D NormalDistribution) x -> cdfSanityCheck d x <?> showError d x  
    log "* CDF is nondecreasing"
    quickCheck $ \(D d :: D NormalDistribution) x y -> cdfIsNondecreasing d x y <?> showError2 d x y
    log "* 1-CDF is correct"
    quickCheck $ \(D d :: D NormalDistribution) x -> cdfComplementIsCorrect d x <?> showError d x 
    log "* CDF at +inf = 1"
    quickCheck $ \(D d :: D NormalDistribution) -> cdfAtPosInfinity d <?> showError d infinity
    log "* CDF at -inf = 0"
    quickCheck $ \(D d :: D NormalDistribution) -> cdfAtNegInfinity d <?> showError d (-1.0/0.0)
    log "* PDF sanity"
    quickCheck $ \(D d :: D NormalDistribution) x -> pdfSanityCheck d x <?> showError d x


-- Uniform distributions
uniformDistrTests :: ∀ eff. QC eff Unit
uniformDistrTests= do
    
    log "* CDF sanity"
    quickCheck $ \(D d :: D UniformDistribution) x -> cdfSanityCheck d x <?> showError d x    
    log "* CDF is nondecreasing"
    quickCheck $ \(D d :: D UniformDistribution) x y -> cdfIsNondecreasing d x y <?> showError2 d x y
    log "* 1-CDF is correct"
    quickCheck $ \(D d :: D UniformDistribution) x -> cdfComplementIsCorrect d x <?> showError d x 
    log "* CDF at +inf = 1"
    quickCheck $ \(D d :: D UniformDistribution) -> cdfAtPosInfinity d <?> showError d infinity
    log "* CDF at -inf = 0"
    quickCheck $ \(D d :: D UniformDistribution) -> cdfAtNegInfinity d <?> showError d (-1.0/0.0)
    log "* PDF sanity"
    quickCheck $ \(D d :: D UniformDistribution) x -> pdfSanityCheck d x <?> showError d x


-- Error message in case test fails
showError :: ∀ t. Show t => t -> Number -> String
showError d x = "Distribution: " <> show d <> ", x: " <> show x <> "\n\n"

-- Error message in case test fails
showError2 :: ∀ t. Show t => t -> Number -> Number -> String
showError2 d x y = "Distribution: " <> show d <> ", x: " <> show x <> ", y: " <> show y <> "\n\n"

-- ------------------------------------------------------------------------------------------------
-- Properties
-- ------------------------------------------------------------------------------------------------

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


-- CDF is in [0,1] range
cdfSanityCheck :: ∀ t. Distribution t => t -> Number -> Boolean
cdfSanityCheck d x = c >= 0.0 && c <= 1.0
    where c = cumulative d x

-- CDF never decreases
cdfIsNondecreasing :: ∀ t. Distribution t => t -> Number -> Number -> Boolean
cdfIsNondecreasing d = monotonicallyIncreases $ cumulative d    

-- CDF's complement is implemented correctly
cdfComplementIsCorrect :: ∀ t. Distribution t => t -> Number -> Boolean
cdfComplementIsCorrect d x = 1.0 ~= (cumulative d x + complCumulative d x)

-- cumulative d +∞ = 1
cdfAtPosInfinity :: ∀ t. ContDistr t => t -> Boolean
cdfAtPosInfinity d = cumulative d infinity == 1.0

-- cumulative d - ∞ = 0
cdfAtNegInfinity :: ∀ t. ContDistr t => t -> Boolean
cdfAtNegInfinity d = cumulative d neginf == 0.0
    where neginf = -1.0/0.0

-- PDF is always positive
pdfSanityCheck :: ∀ t. ContDistr t => t -> Number -> Boolean
pdfSanityCheck d x = (density d x) >= 0.0

    