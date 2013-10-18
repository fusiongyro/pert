module Activity where

import Statistics.Distribution

class HasDuration a where
  toDuration :: a -> IO Double

newtype BasicActivity = BasicActivity (String, Double, [String])

instance HasDuration BasicActivity where
  toDuration (BasicActivity (_, d, _)) = return d

newtype Activity = Activity (String, (Double, Double, Double), [String])

data TimeCalculationMethod = Expected
                           | Gaussian
                           | Beta (Double, Double)

newtype ActivitySimulation = AS (TimeCalculationMethod, Activity)

instance HasDuration ActivitySimulation where
  toDuration (AS (Expected, activity)) = return $ meanDuration activity
  toDuration (AS (Gaussian, activity)) = undefined
  toDuration (AS (Beta _,   activity)) = undefined

meanDuration :: Activity -> Double
meanDuration (Activity (_, (p, ml, o), _)) = (p + 4 * ml + o) / 6

stdDevDuration :: Activity -> Double
stdDevDuration (Activity (_, (p, ml, o), _)) = (p - o) / 6
