-- The normal distribution.  This is a continuous probability
-- distribution that describes data that cluster around a mean.
-- Ported from Haskell statistics package

module Statistics.Distribution.Normal
    ( NormalDistribution
    -- * Constructors
    , normalDistr
    , normalDistrE
    , fromSample
    , standard
    ) where

import Prelude
import Data.Array as A
import Data.Maybe(Maybe(..))
import Math (exp, sqrt, log, pi)
import Numeric.SpecFunctions (erfc)
import Statistics.Distribution (class ContDistr, class Distribution, logDensity)
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
    cumulative (ND {mean, ndCdfDenom}) x = erfc ((mean - x) / ndCdfDenom) / 2.0

    complCumulative (ND {mean, ndCdfDenom}) x = erfc ((x - mean) / ndCdfDenom) / 2.0


-- Contiuous Distribution instance
instance contDistrNormal :: ContDistr NormalDistribution where
    density d = exp <<< logDensity d

    logDensity (ND {mean, stdDev, ndPdfDenom}) x = (-xm * xm / (2.0 * stdDev * stdDev)) - ndPdfDenom
      where xm = x - mean
  

-- | Create normal distribution from mean and standard deviation
-- | Use this function only when you are sure that sd > 0.
-- | Otherwise use safe normalDistrE
normalDistr :: Number -> Number -> NormalDistribution
normalDistr mu sd = ND { mean: mu
                       , stdDev: sd 
                       , ndPdfDenom: log (sqrt (2.0*pi) * sd)
                       , ndCdfDenom: sqrt(2.0) * sd        
                       }


-- | Create normal distribution in a safe way
normalDistrE :: Number -> Number -> Maybe NormalDistribution
normalDistrE mu sd 
  | sd > 0.0 = Just $ ND { mean: mu
                         , stdDev: sd 
                         , ndPdfDenom: log (sqrt (2.0*pi) * sd)
                         , ndCdfDenom: sqrt(2.0) * sd        
                         }
  | otherwise = Nothing


-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = normalDistr 0.0 1.0


-- | Create distribution using parameters estimated from
-- | sample. Variance is estimated using maximum likelihood method
-- | (biased estimation). 
fromSample :: S.Sample -> Maybe NormalDistribution
fromSample xs 
  | A.length xs <= 1 = Nothing
  | otherwise        = if sd == 0.0 then Nothing else Just $ normalDistr mu sd
    where
      mu = S.mean xs
      sd = S.stddev xs

