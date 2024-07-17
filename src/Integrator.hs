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
import Control.Monad.Trans.Reader (reader, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)

integ :: CT Double -> CT Double -> CT (CT Double)
integ diff i =
  mdo y <- memo interpolate z
      z <- do ps <- ask
              let f = solverToFunction (method $ solver ps)
              pure . liftIO $ f diff i y ps
      return y
      
-- | The Integrator type represents an integral with caching.
data Integrator = Integrator { initial :: CT Double,   -- ^ The initial value.
                               cache   :: IORef (CT Double),
                               computation  :: IORef (CT Double)
                             }

initialize :: CT a -> CT a
initialize m = do
  ps <- ask
  if iteration ps == 0 && getSolverStage (stage $ solver ps) == 0 then
    liftIO $ runReaderT m ps
  else
    let iv = interval ps
        sl = solver ps
    in liftIO $ runReaderT m $ ps { time = iterToTime iv sl 0 (SolverStage 0),
                                    iteration = 0,
                                    solver = sl { stage = SolverStage 0 }}

createInteg :: CT Double -> CT Integrator
createInteg i = do
  ps <- ask
  r1 <- liftIO . newIORef $ initialize i 
  r2 <- liftIO . newIORef $ initialize i 
  let integ = Integrator { initial = i, 
                           cache = r1,
                           computation  = r2 }
      z = do ps <- ask
             v <- liftIO $ readIORef (computation integ)
             liftIO $ runReaderT v ps
  y <- liftIO . flip runReaderT ps $ memo interpolate z
  liftIO $ writeIORef (cache integ) y
  pure integ

readInteg :: Integrator -> CT Double
readInteg integ = do
  ps <- ask
  v <- liftIO $ readIORef (cache integ)
  liftIO $ runReaderT v ps

updateInteg :: Integrator -> CT Double -> CT ()
updateInteg integ diff =
  liftIO $ writeIORef (computation integ) z
    where i = initial integ
          z = do ps <- ask
                 v <- liftIO $ readIORef (cache integ)
                 let f = solverToFunction (method $ solver ps)
                 liftIO $ f diff i v ps
    
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
      a <- liftIO $ runReaderT y psy
      b <- liftIO $ runReaderT diff psy
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
                         vy <- liftIO $ runReaderT y psy
                         k1 <- liftIO $ runReaderT f ps1
                         k2 <- liftIO $ runReaderT f ps2
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
                  vy <- liftIO $ runReaderT y psy
                  k1 <- liftIO $ runReaderT f ps1
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
                         vy <- liftIO $ runReaderT y psy
                         k1 <- liftIO $ runReaderT f ps1
                         k2 <- liftIO $ runReaderT f ps2
                         k3 <- liftIO $ runReaderT f ps3
                         k4 <- liftIO $ runReaderT f ps4
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
                  vy <- liftIO $ runReaderT y psy
                  k1 <- liftIO $ runReaderT f ps1
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
                  vy <- liftIO $ runReaderT y psy
                  k2 <- liftIO $ runReaderT f ps2
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
                  vy <- liftIO $ runReaderT y psy
                  k3 <- liftIO $ runReaderT f ps3
                  let !v = vy + dt sl * k3
                  return v
    _ -> 
      error "Incorrect stase: integRK4"
