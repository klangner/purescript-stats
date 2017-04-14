-- The normal distribution.  This is a continuous probability
-- distribution that describes data that cluster around a mean.
-- Ported from Haskell statistics package

module Statistics.Distribution.Normal
    ( NormalDistribution
    -- * Constructors
    , normalDistr
    , fromSample
    , standard
    ) where

import Prelude (class Eq, class Show, negate, show, (&&), (*), (-), (/), (<<<), (<>), (==))
import Math (exp, sqrt, log, pi)
import Statistics.Distribution (class ContDistr, class Distribution, cumulative, logDensity)
import Statistics.Sample as S 


-- | The normal distribution.
data NormalDistribution = ND { mean       :: Number
                             , stdDev     :: Number
                             , ndPdfDenom :: Number
                             , ndCdfDenom :: Number
                             }

-- Show instance
instance showNormal :: Show NormalDistribution where
  show (ND {mean, stdDev}) = "Normal distribution: {mean: " <> show mean <> ", stddev: " <> show stdDev <> "}"

-- Equality instance
instance eqNormal :: Eq NormalDistribution where
  eq (ND {mean: m1, stdDev: s1}) (ND {mean: m2, stdDev: s2}) = m1 == m2 && s1 == s2

-- Distribution instance
instance distrNormal :: Distribution NormalDistribution where
    cumulative d x = 0.0 -- erfc ((mean d - x) / ndCdfDenom d) / 2

    complCumulative d x = 1.0 - cumulative d x -- erfc ((x - mean d) / ndCdfDenom d) / 2    

-- Contiuous Distribution instance
instance contDistrNormal :: ContDistr NormalDistribution where
    density d = exp <<< logDensity d

    logDensity (ND {mean, stdDev, ndPdfDenom}) x = (-xm * xm / (2.0 * stdDev * stdDev)) - ndPdfDenom
      where xm = x - mean
  


-- | Create normal distribution from mean and standard deviation
normalDistr :: Number -> Number -> NormalDistribution
normalDistr mu sd = ND { mean: mu
                       , stdDev: sd 
                       , ndPdfDenom: log (sqrt (2.0*pi) * sd)
                       , ndCdfDenom: sqrt(2.0) * sd        
                       }


-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = normalDistr 0.0 1.0


-- | Create distribution using parameters estimated from
-- | sample. Variance is estimated using maximum likelihood method
-- | (biased estimation). 
fromSample :: S.Sample -> NormalDistribution
fromSample xs = normalDistr mu sd
  where
    mu = S.mean xs
    sd = S.stddev xs

