{-# LANGUAGE RankNTypes #-}
module Duration where

import Control.Monad.Primitive
import Control.Monad.ST

import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

class Duration a where
  duration :: PrimMonad m => a -> GenST (PrimState m) -> m Double

class TimeSimulate a where
  distribution :: forall b. ContGen b => a -> b

--simulateTime :: Duration a => [a] -> GenST [(a, Double)]
data Activity = Activity
                { activityName    :: String
                , optimisticTime  :: Double
                , mostLikelyTime  :: Double
                , pessimisticTime :: Double }
                deriving (Show, Eq)

--instance TimeSimulate Activity where
--  distribution (Activity _ opt ml pess) = normalDistr mean stdDev
--    where
--      mean = (opt + ml * 4 + pess) / 6
--      stdDev = (pess - opt) / 6
    

instance Duration Activity where
  duration (Activity _ opt ml pess) gen = genContinous (normalDistr mean stdDev) gen
    where
      mean   = (opt + ml * 4 + pess) / 6
      stdDev = (pess - opt) / 6

-- withSystemRandom $ asGenIO $ (\gen -> sequence $ map (flip duration gen) (replicate 10000 (Activity "" 15 20 40)))

