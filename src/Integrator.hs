{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}
module Integrator where

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import CT ( Parameters(solver, interval, time, iteration), CT )
import Solver
    ( Solver(dt, method, stage),
      Method(Euler, RungeKutta2, RungeKutta4),
      Stage(SolverStage),
      getSolverStage,
      iterToTime )
import Interpolation ( interpolate )
import Memo ( memo )
import Control.Monad.Trans.Reader ( ReaderT(ReaderT, runReaderT) )

integ :: CT Double -> CT Double -> CT (CT Double)
integ diff i =
  mdo y <- memo interpolate z
      z <- ReaderT $ \ps ->
              let f = solverToFunction (method $ solver ps)
              in pure . ReaderT $ f diff i y
      return y
      
-- | The Integrator type represents an integral with caching.
data Integrator = Integrator { initial :: CT Double,   -- ^ The initial value.
                               cache   :: IORef (CT Double),
                               computation  :: IORef (CT Double)
                             }

initialize :: CT a -> CT a
initialize m =
  ReaderT $ \ps ->
  if iteration ps == 0 && getSolverStage (stage $ solver ps) == 0 then
    runReaderT m ps
  else
    let iv = interval ps
        sl = solver ps
    in runReaderT m $ ps { time = iterToTime iv sl 0 (SolverStage 0),
                           iteration = 0,
                           solver = sl { stage = SolverStage 0 }}

createInteg :: CT Double -> CT Integrator
createInteg i =
  ReaderT $ \ps ->
    do r1 <- newIORef $ initialize i 
       r2 <- newIORef $ initialize i 
       let integ = Integrator { initial = i, 
                                cache = r1,
                                computation  = r2 }
           z = ReaderT $ \ps ->
             do v <- readIORef (computation integ)
                runReaderT v ps
       y <- runReaderT (memo interpolate z) ps
       writeIORef (cache integ) y
       return integ

readInteg :: Integrator -> CT Double
readInteg integ = 
  ReaderT $ \ps -> flip runReaderT ps =<< readIORef (cache integ)

updateInteg :: Integrator -> CT Double -> CT ()
updateInteg integ diff =
  ReaderT . const $ writeIORef (computation integ) z
    where i = initial integ
          z = ReaderT $ \ps ->
                let f = solverToFunction (method $ solver ps)
                in
                (\y -> f diff i y ps) =<< readIORef (cache integ)

solverToFunction Euler = integEuler
solverToFunction RungeKutta2 = integRK2
solverToFunction RungeKutta4 = integRK4

integEuler :: CT Double
             -> CT Double 
             -> CT Double 
             -> Parameters -> IO Double
integEuler diff i y ps =
  case iteration ps of
    0 -> 
      runReaderT i ps
    n -> do 
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) (SolverStage 0)
          psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0} }
      a <- runReaderT y psy
      b <- runReaderT diff psy
      let !v = a + dt (solver ps) * b
      return v

integRK2 :: CT Double
           -> CT Double
           -> CT Double
           -> Parameters -> IO Double
integRK2 f i y ps =
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 ->
                         runReaderT i ps
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl (n - 1) (SolverStage 1)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                         vy <- runReaderT y psy
                         k1 <- runReaderT f ps1
                         k2 <- runReaderT f ps2
                         let !v = vy + dt sl / 2.0 * (k1 + k2)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- runReaderT y psy
                  k1 <- runReaderT f ps1
                  let !v = vy + dt sl * k1
                  return v
    _ -> 
      error "Incorrect stage: integRK2"

integRK4 :: CT Double
           -> CT Double
           -> CT Double
           -> Parameters -> IO Double
integRK4 f i y ps =
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 -> 
                         runReaderT i ps
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl  (n - 1) (SolverStage 1)
                             t3 = iterToTime iv sl  (n - 1) (SolverStage 2)
                             t4 = iterToTime iv sl  (n - 1) (SolverStage 3)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                             ps3 = ps { time = t3, iteration = n - 1, solver = sl { stage = SolverStage 2 }}
                             ps4 = ps { time = t4, iteration = n - 1, solver = sl { stage = SolverStage 3 }}
                         vy <- runReaderT y psy
                         k1 <- runReaderT f ps1
                         k2 <- runReaderT f ps2
                         k3 <- runReaderT f ps3
                         k4 <- runReaderT f ps4
                         let !v = vy + dt sl / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- runReaderT y psy
                  k1 <- runReaderT f ps1
                  let !v = vy + dt sl / 2.0 * k1
                  return v
    SolverStage 2 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t2 = iterToTime iv sl n (SolverStage 1)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps2 = ps { time = t2, iteration = n, solver = sl { stage = SolverStage 1 }}
                  vy <- runReaderT y psy
                  k2 <- runReaderT f ps2
                  let !v = vy + dt sl / 2.0 * k2
                  return v
    SolverStage 3 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t3 = iterToTime iv sl n (SolverStage 2)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps3 = ps { time = t3, iteration = n, solver = sl { stage = SolverStage 2 }}
                  vy <- runReaderT y psy
                  k3 <- runReaderT f ps3
                  let !v = vy + dt sl * k3
                  return v
    _ -> 
      error "Incorrect stase: integRK4"
