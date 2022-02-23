module Solver where

import Prelude hiding (Real)
import Types
import Simulation

-- | It defines configurations to use within the solver
data Solver = Solver { iteration :: Iteration,    -- ^ the current iteration
                       dt        :: Real, -- ^ the integration time step
                       method    :: Method, -- ^ the integration method
                       stage     :: Stage     -- ^ the current stage
                     } deriving (Eq, Ord, Show)

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

-- | Make a list of all possible stages, based on the solver method
stages :: Solver -> [Stage]
stages sl = 
  case method sl of
    Euler -> [0]
    RungeKutta2 -> [0, 1]
    RungeKutta4 -> [0, 1, 2, 3]

-- | Make a tuple with minimum and maximum boundaries, based on the solver method
stageBnds :: Solver -> (Stage, Stage)
stageBnds sl = 
  case method sl of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

-- | Auxiliary functions for boundaries of stages
stageLoBnd :: Solver -> Stage
stageLoBnd sc = fst $ stageBnds sc
                  
stageHiBnd :: Solver -> Stage
stageHiBnd sc = snd $ stageBnds sc

-- | Transforms iteration to time
iterToTime :: Interval -> Solver -> Iteration -> Stage -> Real
iterToTime interv solver n st =
  if st < 0 then 
    error "Incorrect stage: iterToTime"
  else
    (startTime interv) + n' * (dt solver) + delta (method solver) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt solver
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt solver / 2
            delta RungeKutta4 2 = dt solver / 2
            delta RungeKutta4 3 = dt solver
