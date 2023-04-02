{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds, MonoLocalBinds, MultiParamTypeClasses, RankNTypes, QuantifiedConstraints #-}

module Memo where

import CT
import Solver
import Simulation

import Data.IORef
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Types

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


-- -- | The 'Memo' class specifies a type for which an array can be created.
class (MArray (STArray s) e (ST s)) => (Memo' s) e where
  newMemoArray_' :: Ix i => (i, i) -> ST s (STArray s i e)

-- -- | The 'UMemo' class specifies a type for which an unboxed array exists.
class (MArray (STUArray s) e (ST s)) => (UMemo' s) e where
  newMemoUArray_' :: Ix i => (i, i) -> ST s (STUArray s i e)

instance (Memo' s) e where
  newMemoArray_' = newArray_
    
instance (MArray (STUArray s) e (ST s)) => (UMemo' s) e where
  newMemoUArray_' = newArray_


-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
memo :: UMemo e => (CT e -> CT e) -> CT e 
        -> CT (CT e)

memo tr (CT m) = 
  CT $ \ps ->
  do let sl = solver ps
         iv = interval ps
         (SolverStage stl, SolverStage stu) = stageBnds sl
         (nl, nu)   = iterationBnds iv (dt sl)
     arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps =
           do let sl  = solver ps
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
                      in do a <- m ps'
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
     return $ tr $ CT r
     
-- (\ps -> IO (\ps -> IO e))
memo' :: (forall s. (UMemo' s) e) => (CT' e -> CT' e) -> CT' (ST s e) -> CT' (ST s (CT' (ST s e)))
memo' tr (CT' m) =
  CT' $ \ps ->
  do let sl = solver ps
         iv = interval ps
         (SolverStage stl, SolverStage stu) = stageBnds sl
         (nl, nu)   = iterationBnds iv (dt sl)
     arr   <- newMemoUArray_' ((stl, nl), (stu, nu))
     nref  <- newSTRef 0
     stref <- newSTRef 0
     r' <- pure $ r arr stref nref m 
     pure $ CT' r'

r :: MArray a b (ST s) => a (Int, Iteration) b -> STRef s Int -> STRef s Iteration -> (Parameters -> ST s b) -> Parameters -> ST s b
r arr stref nref m ps =
  do let sl  = solver ps
         iv  = interval ps
         n   = iteration ps
         st  = getSolverStage $ stage sl
         stu = getSolverStage $ stageHiBnd sl 
     n'  <- readSTRef nref
     st' <- readSTRef stref
     loop' <- pure $ loop n' st' arr n st ps iv sl m stu stref nref
     loop'

loop :: MArray a b (ST s) => Iteration -> Int -> a (Int, Iteration) b -> Iteration -> Int -> Parameters -> Interval -> Solver -> (Parameters -> ST s b) -> Int -> STRef s Int -> STRef s Iteration -> ST s b
loop n' st' arr n st ps iv sl m stu stref nref =  
          if (n' > n) || ((n' == n) && (st' > st)) 
          then 
            readArray arr (st, n)
          else 
            let ps' = ps { time = iterToTime iv sl n' (SolverStage st'),
                           iteration = n',
                           solver = sl { stage = SolverStage st' }}
            in do a <- m ps
                  a `seq` writeArray arr (st', n') a
                  if st' >= stu 
                    then do writeSTRef stref 0
                            writeSTRef nref (n' + 1)
                            loop (n' + 1) 0 arr n st ps iv sl m stu stref nref
                    else do writeSTRef stref (st' + 1)
                            loop n' (st' + 1) arr n st ps iv sl m stu stref nref

-- createTable :: (Monad m, MArray (STUArray s) e (ST s), Num a1, Num a2) => CT' a3 -> m (CT' (ST s (STUArray s (Int, Types.Iteration) e, STRef s a1, STRef s a2)))
createTable :: (MArray (STUArray s) e (ST s), Num a1, Num a2) => CT' a3 -> CT' (ST s (STUArray s (Int, Iteration) e, STRef s a1, STRef s a2))
createTable (CT' m) =
  CT' $ \ps ->
  do let sl = solver ps
         iv = interval ps
         (SolverStage stl, SolverStage stu) = stageBnds sl
         (nl, nu)   = iterationBnds iv (dt sl)
     arr   <- newMemoUArray_' ((stl, nl), (stu, nu))
     nref  <- newSTRef 0
     stref <- newSTRef 0
     return (arr, nref, stref)


-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (CT e -> CT e) -> CT e 
        -> CT (CT e)
memo0 tr (CT m) = 
  CT $ \ps ->
  do let iv   = interval ps
         bnds = iterationBnds iv (dt (solver ps))
     arr   <- newMemoArray_ bnds
     nref  <- newIORef 0
     let r ps =
           do let sl = solver ps
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
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ CT r
