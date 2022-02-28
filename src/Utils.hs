module Utils where

import Prelude hiding (Real)
import Types
import Dynamics
import Simulation
import Solver

-- | Function to solve floating point approximations
neighborhood :: Solver -> Real -> Real -> Bool
neighborhood sl t t' = 
  abs (t - t') <= dt sl / 1.0e6

-- | Discretize the computation in the integration time points.
discrete :: Dynamics a -> Dynamics a
discrete (Dynamics m) =
  Dynamics $ \ps ->
  let st = stage (solver ps)
      r | st == 0    = m ps
        | st > 0    = let iv = interval ps
                          sl = solver ps
                          n  = iteration sl
                      in m $ ps { time = iterToTime iv sl n 0,
                                  solver = sl {stage = 0} }
        | otherwise = let iv = interval ps
                          t  = time ps
                          sl = solver ps
                          n  = iteration sl
                          t' = startTime iv + fromIntegral (n + 1) * dt sl
                          n' = if neighborhood sl t t' then n + 1 else n
                      in m $ ps { time = iterToTime iv sl n' 0,
                                  solver = sl { iteration = n',
                                                stage = 0 }}
  in r

-- | Interpolate the computation based on the integration time points only.
interpolate :: Dynamics Double -> Dynamics Double
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if stage (solver ps) >= 0 then 
    m ps
  else 
    let iv = interval ps
        sl = solver ps
        t  = time ps
        st = dt sl
        x  = (t - startTime iv) / st
        n1 = max (floor x) (iterationLoBnd iv st)
        n2 = min (ceiling x) (iterationHiBnd iv st)
        t1 = iterToTime iv sl n1 0
        t2 = iterToTime iv sl n2 0
        z1 = m $ ps { time = t1, 
                      solver = sl { iteration = n1, 
                                    stage = 0 }}
        z2 = m $ ps { time = t2,
                      solver = sl { iteration = n2,
                                    stage = 0 }}
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r