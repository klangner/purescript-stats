-- | Type classes for probability distributions
module Statistics.Distribution where



-- | Type class common to all distributions. Only c.d.f. could be
-- | defined for both discrete and continous distributions.
class Distribution d where
    -- | Cumulative distribution function.  The probability that a
    -- | random variable /X/ is less or equal than /x/,
    -- | i.e. P(/X/&#8804;/x/). Cumulative should be defined for
    -- | infinities as well:
    --
    -- > cumulative d +∞ = 1
    -- > cumulative d -∞ = 0
    cumulative :: d -> Number -> Number

    -- | One's complement of cumulative distibution:
    --
    -- > complCumulative d x = 1 - cumulative d x
    complCumulative :: d -> Number -> Number
    -- complCumulative d x = 1 - cumulative d x


-- | Discrete probability distribution.
class Distribution  d <= DiscreteDistr d where
    -- | Probability of n-th outcome.
    probability :: d -> Int -> Number
    -- probability d = exp . logProbability d

    -- | Logarithm of probability of n-th outcome
    logProbability :: d -> Int -> Number
    -- logProbability d = log . probability d


-- | Continuous probability distributuion.
--
-- |  Minimal complete definition is 'quantile' and either 'density' or
-- |  'logDensity'.
class Distribution d <= ContDistr d where
    -- | Probability density function. Probability that random
    -- | variable /X/ lies in the infinitesimal interval
    -- | [/x/,/x+/&#948;/x/) equal to /density(x)/&#8901;&#948;/x/
    density :: d -> Number -> Number
    -- density d = exp . logDensity d

    -- | Inverse of the cumulative distribution function. The value
    -- | /x/ for which P(/X/&#8804;/x/) = /p/. If probability is outside
    -- | of [0,1] range function should call 'error'
    -- quantile :: d -> Number -> Number

    -- | Natural logarithm of density.
    logDensity :: d -> Number -> Number
    -- logDensity d = log . density d


