module Driver where

import CT
import Solver
import Simulation
import Types

type Model a = CT (CT a)

epslon = 0.00001

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runCTFinal :: Model a -> Double -> Solver -> IO a
runCTFinal (CT m) t sl = 
  do d <- m Parameters { interval = Interval 0 t,
                         time = 0,
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}
     subRunCTFinal d t sl

-- | Auxiliary functions to runCTFinal
subRunCTFinal :: CT a -> Double -> Solver -> IO a
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
runCT :: Model a -> Double -> Solver -> IO [a]
runCT (CT m) t sl = do
  d <- m Parameters { interval = Interval 0 t,
                      time = 0,
                      iteration = 0,
                      solver = sl { stage = SolverStage 0}}
  sequence $ subRunCT d t sl

-- | Auxiliary functions to runCT
subRunCT :: CT a -> Double -> Solver -> [IO a]
subRunCT (CT m) t sl = do
  let iv = Interval 0 t
      (nl, nu) = iterationBnds iv (dt sl)
      parameterize n =
        let time = iterToTime iv sl n (SolverStage 0)
            solver = sl {stage = SolverStage 0}
        in
        Parameters {interval = iv,
                    time = time,
                    iteration = n,
                    solver = solver}
      ps =
       Parameters {interval = iv,
                   time = t,
                   iteration = nu,
                   solver = sl {stage = Interpolate}}
      endTime = iterToTime iv sl nu (SolverStage 0)
      values = map (m . parameterize) [nl .. nu]
  if endTime - t < epslon
  then values
  else init values ++ [m ps]

