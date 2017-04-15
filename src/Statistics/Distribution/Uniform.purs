-- Variate distributed uniformly in the interval.

module Statistics.Distribution.Uniform
    ( UniformDistribution(..)
    -- * Constructors
    , uniformDistr
    , uniformDistrE
    ) where

import Prelude
import Data.Maybe(Maybe(..))
import Math (log)
import Statistics.Distribution (class ContDistr, class Distribution, cumulative, density)


-- | Uniform distribution from A to B
data UniformDistribution = UD { uniformA :: Number -- ^ Low boundary of distribution
                              , uniformB :: Number -- ^ Upper boundary of distribution
                              }

-- Show instance
instance showUniform :: Show UniformDistribution where
  show (UD {uniformA, uniformB}) = "Uniform distribution: {A: " <> show uniformA <> ", B: " <> show uniformB <> "}"

-- Equality instance
instance eqNormal :: Eq UniformDistribution where
  eq (UD {uniformA: a1, uniformB: b1}) (UD {uniformA: a2, uniformB: b2}) = a1 == a2 && b1 == b2

-- Distribution instance
instance distrUniform :: Distribution UniformDistribution where
    cumulative (UD {uniformA, uniformB}) x
      | x < uniformA  = 0.0
      | x > uniformB  = 1.0
      | otherwise     = (x - uniformA) / (uniformB - uniformA)

    complCumulative d x = 1.0 - cumulative d x

-- Contiuous Distribution instance
instance contDistrUniform :: ContDistr UniformDistribution where
    density (UD {uniformA, uniformB}) x
      | x < uniformA     = 0.0
      | x > uniformB     = 0.0
      | otherwise = 1.0 / (uniformB - uniformA)

    logDensity d = log <<< density d
  

-- | Create normal distribution from mean and standard deviation
-- | Use this function only when you are sure that sd > 0.
-- | Otherwise use safe normalDistrE
uniformDistr :: Number -> Number -> UniformDistribution
uniformDistr a b = UD {uniformA: a, uniformB: b}


-- | Create normal distribution in a safe way
uniformDistrE :: Number -> Number -> Maybe UniformDistribution
uniformDistrE a b
  | b < a     = Just $ UD {uniformA: b, uniformB: a}
  | a < b     = Just $ UD {uniformA: a, uniformB: b}
  | otherwise = Nothing

