module Driver where

import CT
import Solver
import Simulation

type Model a = CT (CT a)

epslon = 0.00001

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runCTFinal :: Model a -> Interval -> Solver -> IO a
runCTFinal (CT m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}
     subRunCTFinal d iv sl

-- | Auxiliary functions to runDyanamics (individual computation and list of computations)
subRunCTFinal :: CT a -> Interval -> Solver -> IO a
subRunCTFinal (CT m) iv sl =
  do let n = iterationHiBnd iv (dt sl)
         t = iterToTime iv sl n (SolverStage 0)
         x = m Parameters { interval = iv,
                            time = t,
                            iteration = n,
                            solver = sl { stage = SolverStage 0 }}
     if t - stopTime iv < epslon
     then x
     else m Parameters { interval = iv,
                         time = stopTime iv,
                         iteration = n,
                         solver = sl { stage = Interpolate }}

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runCT :: Model a -> Interval -> Solver -> IO [a]
runCT (CT m) iv sl = 
  do d <- m Parameters { interval = iv,
                         time = startTime iv,
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}
     sequence $ subRunCT d iv sl

subRunCT :: CT a -> Interval -> Solver -> [IO a]
subRunCT (CT m) iv sl =
  do let (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n (SolverStage 0),
                                       iteration = n,
                                       solver = sl { stage = SolverStage 0 }}
         ps = Parameters { interval = iv,
                           time = stopTime iv,
                           iteration = nu,
                           solver = sl { stage = Interpolate }}
     if iterToTime iv sl nu (SolverStage 0) - stopTime iv < epslon
     then map (m . parameterise) [nl .. nu]
     else init $ map (m . parameterise) [nl .. nu] ++ [m ps]     

type BasicModel a = CT a

basicRunCT :: BasicModel a -> Interval -> Solver -> IO [a]
basicRunCT (CT m) iv sl =
  do let (nl, nu) = basicIterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n (SolverStage 0),
                                       iteration = n,
                                       solver = sl { stage = SolverStage 0 }}
         ps = Parameters { interval = iv,
                           time = stopTime iv,
                           iteration = nu,
                           solver = sl { stage = Interpolate }}
     if iterToTime iv sl nu (SolverStage 0) - stopTime iv < epslon
     then sequence $ map (m . parameterise) [nl .. nu]
     else sequence (init $ map (m . parameterise) [nl .. nu] ++ [m ps])

