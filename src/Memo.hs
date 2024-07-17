{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds, MonoLocalBinds #-}

module Memo where

import CT
import Solver
import Simulation

import Data.IORef
import Data.Array
import Data.Array.IO
import Control.Monad.Trans.Reader (reader, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)

-- -- | The 'Memo' class specifies a type for which an array can be created.
class (MArray IOArray e IO) => Memo e where
  newMemoArray_ :: Ix i => (i, i) -> IO (IOArray i e)

-- -- | The 'UMemo' class specifies a type for which an unboxed array exists.
class (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance Memo e where
  newMemoArray_ = newArray_
    
instance (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ = newArray_

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
memo :: UMemo e => (CT e -> CT e) -> CT e 
        -> CT (CT e)
memo tr m = do 
  ps <- ask
  let sl = solver ps
      iv = interval ps
      (SolverStage stl, SolverStage stu) = stageBnds sl
      (nl, nu)   = iterationBnds iv (dt sl)
  arr   <- liftIO $ newMemoUArray_ ((stl, nl), (stu, nu))
  nref  <- liftIO $ newIORef 0
  stref <- liftIO $ newIORef 0
  let r = do
        ps <- ask
        let sl  = solver ps
            iv  = interval ps
            n   = iteration ps
            st  = getSolverStage $ stage sl
            stu = getSolverStage $ stageHiBnd sl 
            loop n' st' = 
              if (n' > n) || ((n' == n) && (st' > st)) 
              then 
                readArray arr (st, n)
              else 
                let ps' = ps { time = iterToTime iv sl n' (SolverStage st'),
                               iteration = n',
                               solver = sl { stage = SolverStage st' }}
                in do a <- runReaderT m ps'
                      a `seq` writeArray arr (st', n') a
                      if st' >= stu 
                        then do writeIORef stref 0
                                writeIORef nref (n' + 1)
                                loop (n' + 1) 0
                        else do writeIORef stref (st' + 1)
                                loop n' (st' + 1)
        n'  <- liftIO $ readIORef nref
        st' <- liftIO $ readIORef stref
        liftIO $ loop n' st'
  pure . tr $ r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (CT e -> CT e) -> CT e 
        -> CT (CT e)
memo0 tr m = do
  ps <- ask
  let iv   = interval ps
      bnds = iterationBnds iv (dt (solver ps))
  arr   <- liftIO $ newMemoArray_ bnds
  nref  <- liftIO $ newIORef 0
  let r = do
        ps <- ask
        let sl = solver ps
            iv = interval ps
            n  = iteration ps
            loop n' = 
              if n' > n
              then 
                readArray arr n
              else 
                let ps' = ps { time = iterToTime iv sl n' (SolverStage 0),
                               iteration = n',
                               solver = sl { stage = SolverStage 0} }
                in do a <- runReaderT m ps'
                      a `seq` writeArray arr n' a
                      writeIORef nref (n' + 1)
                      loop (n' + 1)
        n' <- liftIO $ readIORef nref
        liftIO $ loop n'
  pure . tr $ r
