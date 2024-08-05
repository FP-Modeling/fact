module Interpolation where

import CT ( Parameters(solver, interval, time, iteration), CT )
import Simulation
    ( Interval(startTime), iterationLoBnd, iterationHiBnd )
import Solver
    ( Solver(stage, dt),
      Stage(SolverStage, Interpolate),
      getSolverStage,
      iterToTime )
import Control.Monad.Trans.Reader ( ReaderT(ReaderT, runReaderT) )

-- | Function to solve floating point approximations
neighborhood :: Solver -> Double -> Double -> Bool
neighborhood sl t t' = 
  abs (t - t') <= dt sl / 1.0e6

-- | Discretize the computation in the integration time points.
discrete :: CT a -> CT a
discrete m = 
  ReaderT $ \ps ->
  let st = getSolverStage $ stage (solver ps)
      r | st == 0    = runReaderT m ps
        | st > 0    = let iv = interval ps
                          sl = solver ps
                          n  = iteration ps
                      in runReaderT m $ ps { time = iterToTime iv sl n (SolverStage 0),
                                             solver = sl {stage = SolverStage 0} }
        | otherwise = let iv = interval ps
                          t  = time ps
                          sl = solver ps
                          n  = iteration ps
                          t' = startTime iv + fromIntegral (n + 1) * dt sl
                          n' = if neighborhood sl t t' then n + 1 else n
                      in runReaderT m $ ps { time = iterToTime iv sl n' (SolverStage 0),
                                             iteration = n',
                                             solver = sl { stage = SolverStage 0} }
  in r

-- | Interpolate the computation based on the integration time points only.
interpolate :: CT Double -> CT Double
interpolate m =
  ReaderT $ \ps ->
  case stage $ solver ps of
    SolverStage _ -> runReaderT m ps
    Interpolate   ->
      let iv = interval ps
          sl = solver ps
          t  = time ps
          st = dt sl
          x  = (t - startTime iv) / st
          n1 = max (floor x) (iterationLoBnd iv st)
          n2 = min (ceiling x) (iterationHiBnd iv st)
          t1 = iterToTime iv sl n1 (SolverStage 0)
          t2 = iterToTime iv sl n2 (SolverStage 0)
          z1 =
            runReaderT m $ ps { time = t1,
                                iteration = n1,
                                solver = sl { stage = SolverStage 0 }}
          z2 =
            runReaderT m $ ps { time = t2,
                                iteration = n2,
                                solver = sl { stage = SolverStage 0 }}         
      in do y1 <- z1
            y2 <- z2
            return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
