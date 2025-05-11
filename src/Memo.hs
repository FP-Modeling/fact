-- Copyright (c) 2009, 2010, 2011 David Sorokin <david.sorokin@gmail.com>
-- Copyright (c) 2021-2025 Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- |
-- Module     : Memo
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
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
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

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
-- the specified interpolation and being aware of the Runge-Kutta method.m
memo :: UMemo e => (CT e -> CT e) -> CT e -> CT (CT e)
memo interpolate m = do
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
            loop n' stage' =
              -- we have already memoized all values from 0 to n and from 0 to
              -- st, just look up.
              if (n' > n) || ((n' == n) && (stage' > st))
              then 
                readArray arr (st, n)
              else 
                let ps' = ps { time = iterToTime iv sl n' (SolverStage stage'),
                               iteration = n',
                               solver = sl { stage = SolverStage stage' }}
                in do a <- runReaderT m ps'
                      a `seq` writeArray arr (stage', n') a
                      if stage' >= stu
                        then do writeIORef stref 0
                                writeIORef nref (n' + 1)
                                loop (n' + 1) 0
                        else do writeIORef stref (stage' + 1)
                                loop n' (stage' + 1)
        n'  <- liftIO $ readIORef nref
        st' <- liftIO $ readIORef stref
        liftIO $ loop n' st'
  pure . interpolate $ r
