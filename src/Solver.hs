module Solver where

import Types ( Iteration )
import Simulation ( Interval(startTime) )

-- | It defines configurations to use within the solver
data Solver = Solver { dt        :: Double,      -- ^ the integration time step
                       method    :: Method,    -- ^ the integration method
                       stage     :: Stage      -- ^ the current stage
                     } deriving (Eq, Ord, Show)

data Stage = SolverStage Int
           | Interpolate
           deriving (Eq, Ord, Show)

getSolverStage :: Stage -> Int
getSolverStage (SolverStage st) = st
getSolverStage Interpolate = 0

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

-- | Make a list of all possible stages, based on the solver method
stages :: Solver -> [Stage]
stages sl = 
  case method sl of
    Euler -> solverStage [0]
    RungeKutta2 -> solverStage [0, 1]
    RungeKutta4 -> solverStage [0, 1, 2, 3]
  where solverStage list = map SolverStage list

-- | Make a tuple with minimum and maximum boundaries, based on the solver method
stageBnds :: Solver -> (Stage, Stage)
stageBnds sl = 
  case method sl of
    Euler -> solverStage (0, 0)
    RungeKutta2 -> solverStage (0, 1)
    RungeKutta4 -> solverStage (0, 3)
  where solverStage (init, end) = (SolverStage init, SolverStage end)

-- | Auxiliary functions for boundaries of stages
stageLoBnd :: Solver -> Stage
stageLoBnd sc = fst $ stageBnds sc
                  
stageHiBnd :: Solver -> Stage
stageHiBnd sc = snd $ stageBnds sc

-- | Transforms iteration to time
iterToTime :: Interval -> Solver -> Iteration -> Stage -> Double
iterToTime _ _ _ Interpolate = error "Incorrect stage: Interpolate"
iterToTime interv solver n (SolverStage st) =
  if st < 0 then 
    error "Incorrect solver stage in iterToTime"
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
