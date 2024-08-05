{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds, MonoLocalBinds #-}

module Memo where

import CT ( CT, Parameters(solver, interval, time, iteration) )
import Solver
    ( getSolverStage,
      iterToTime,
      stageBnds,
      stageHiBnd,
      Solver(stage, dt),
      Stage(SolverStage) )
import Simulation ( iterationBnds )

import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Array ( Ix )
import Data.Array.IO
    ( Ix, readArray, writeArray, MArray(newArray_), IOUArray, IOArray )
import Control.Monad.Trans.Reader ( ReaderT(ReaderT, runReaderT) )

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
memo tr m =
  ReaderT $ \ps -> do
  let sl = solver ps
      iv = interval ps
      (SolverStage stl, SolverStage stu) = stageBnds sl
      (nl, nu)   = iterationBnds iv (dt sl)
  arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
  nref  <- newIORef 0
  stref <- newIORef 0
  let r ps = do
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
        n'  <- readIORef nref
        st' <- readIORef stref
        loop n' st'
  pure . tr . ReaderT $ r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (CT e -> CT e) -> CT e 
        -> CT (CT e)
memo0 tr m =
  ReaderT $ \ps -> do
  let iv   = interval ps
      bnds = iterationBnds iv (dt (solver ps))
  arr   <- newMemoArray_ bnds
  nref  <- newIORef 0
  let r ps = do
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
        n' <- readIORef nref
        loop n'
  pure . tr . ReaderT $ r
