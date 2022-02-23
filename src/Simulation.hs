module Simulation where

import Prelude hiding (Real)
import Types

-- | It defines a time interval
data Interval = Interval { startTime :: Double, -- ^ the start time
                           stopTime  :: Double  -- ^ the stop time
                         } deriving (Eq, Ord, Show)

-- | Make a list of all possible iterations from 0
iterations :: Interval -> TimeStep -> [Iteration]
iterations interv dt = [i1 .. i2] where
  i1 = 0
  i2 = round ((stopTime interv - 
               startTime interv) / dt)

-- | Make a tuple with minium and maximum boundaries
iterationBnds :: Interval -> TimeStep -> (Iteration, Iteration)
iterationBnds interv dt = (0, round ((stopTime interv - 
                               startTime interv) / dt))

-- | Auxiliary functions for boundaries of iterations                   
iterationLoBnd :: Interval -> TimeStep -> Iteration
iterationLoBnd interv dt = fst $ iterationBnds interv dt

iterationHiBnd :: Interval -> TimeStep -> Iteration
iterationHiBnd interv dt = snd $ iterationBnds interv dt                   


