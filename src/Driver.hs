module Driver where

import CT
import Solver
import Simulation
import Prelude hiding (Real)
import Types

type Model a = CT (CT a)

epslon = 0.00001

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runCTFinal :: Model a -> Real -> Solver -> IO a
runCTFinal (CT m) t sl = 
  do d <- m Parameters { interval = Interval 0 t,
                         time = 0,
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}
     subRunCTFinal d t sl

-- | Auxiliary functions to runDyanamics (individual computation and list of computations)
subRunCTFinal :: CT a -> Real -> Solver -> IO a
subRunCTFinal (CT m) t sl =
  do let iv = Interval 0 t
         n = iterationHiBnd iv (dt sl)
         disct = iterToTime iv sl n (SolverStage 0)
         x = m Parameters { interval = iv,
                            time = disct,
                            iteration = n,
                            solver = sl { stage = SolverStage 0 }}
     if disct - t < epslon
     then x
     else m Parameters { interval = iv,
                         time = t,
                         iteration = n,
                         solver = sl { stage = Interpolate }}

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runCT :: Model a -> Real -> Solver -> IO [a]
runCT (CT m) t sl = 
  do d <- m Parameters { interval = Interval 0 t,
                         time = 0,
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}
     sequence $ subRunCT d t sl

subRunCT :: CT a -> Real -> Solver -> [IO a]
subRunCT (CT m) t sl =
  do let iv = Interval 0 t
         (nl, nu) = iterationBnds iv (dt sl)
         parameterise n = Parameters { interval = iv,
                                       time = iterToTime iv sl n (SolverStage 0),
                                       iteration = n,
                                       solver = sl { stage = SolverStage 0 }}
         ps = Parameters { interval = iv,
                           time = t,
                           iteration = nu,
                           solver = sl { stage = Interpolate }}
     if iterToTime iv sl nu (SolverStage 0) - t < epslon
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

