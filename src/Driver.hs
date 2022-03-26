module Driver where

import Dynamics
import Solver
import Simulation

type Model a = Dynamics (Dynamics a)

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runDynamicsFinal :: Model a -> Interval -> Solver -> IO a
runDynamicsFinal (Dynamics m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = 0 }}
     subRunDynamicsFinal d iv sl

-- | Auxiliary functions to runDyanamics (individual computation and list of computations)
subRunDynamicsFinal :: Dynamics a -> Interval -> Solver -> IO a
subRunDynamicsFinal (Dynamics m) iv sl =
  do let n = iterationHiBnd iv (dt sl)
         t = iterToTime iv sl n 0
     m Parameters { interval = iv,
                    time = t,
                    iteration = n,
                    solver = sl { stage = 0 }}

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamics :: Model a -> Interval -> Solver -> IO [a]
runDynamics (Dynamics m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = 0 }}
     sequence $ subRunDynamics d iv sl

subRunDynamics :: Dynamics a -> Interval -> Solver -> [IO a]
subRunDynamics (Dynamics m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n 0,
                                       iteration = n,
                                       solver = sl { stage = 0 }}
     map (m . parameterise) [nl .. nu]

