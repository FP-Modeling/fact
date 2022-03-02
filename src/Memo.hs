{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds, MonoLocalBinds #-}

module Memo where

import Dynamics
import Solver
import Simulation

import Data.IORef
import Data.Array
import Data.Array.IO

-- --
-- -- Memoization
-- --
-- -- The memoization creates such processes, which values are 
-- -- defined and then stored in the cache for the points of
-- -- integration. You should use some kind of interpolation 
-- -- like the interpolate function to process all other time 
-- -- points that don't coincide with the integration points:
-- --
-- --   x = memo interpolate y    -- a linear interpolation
-- --   x = memo discrete y       -- a discrete process
-- --

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
memo :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
memo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sl = solver ps
         iv = interval ps
         (stl, stu) = stageBnds sl
         (nl, nu)   = iterationBnds iv (dt sl)
     arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps =
           do let sl  = solver ps
                  iv  = interval ps
                  n   = iteration ps
                  st  = stage sl
                  stu = stageHiBnd sl 
                  loop n' st' = 
                    if (n' > n) || ((n' == n) && (st' > st)) 
                    then 
                      readArray arr (st, n)
                    else 
                      let ps' = ps { time = iterToTime iv sl n' st',
                                     iteration = n',
                                     solver = sl { stage = st' }}
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
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
memo0 tr (Dynamics m) = 
  Dynamics $ \ps ->
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
                      let ps' = ps { time = iterToTime iv sl n' 0,
                                     iteration = n',
                                     solver = sl { stage = 0} }
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ Dynamics r
