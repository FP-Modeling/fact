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
-- Module     : Driver
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Driver where

import CT
    ( CT, Parameters(Parameters, solver, interval, time, iteration) )
import Solver
    ( Solver(stage, dt), Stage(SolverStage, Interpolate), iterToTime )
import Simulation
    ( Interval(Interval), iterationHiBnd, iterationBnds )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT) )

type Model a = CT (CT a)

epslon = 0.00001

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runCTFinal :: Model a -> Double -> Solver -> IO a
runCTFinal m t sl = do
  d <- runReaderT m $ Parameters { interval = Interval 0 t,
                                   time = 0,
                                   iteration = 0,
                                   solver = sl { stage = SolverStage 0 }}
  subRunCTFinal d t sl

-- | Auxiliary functions to runCTFinal
subRunCTFinal :: CT a -> Double -> Solver -> IO a
subRunCTFinal m t sl = do
  let iv = Interval 0 t
      n = iterationHiBnd iv (dt sl)
      disct = iterToTime iv sl n (SolverStage 0)
      ps = if disct - t < epslon
           then Parameters { interval = iv,
                             time = disct,
                             iteration = n,
                             solver = sl { stage = SolverStage 0 }}
           else Parameters { interval = iv,
                             time = t,
                             iteration = n,
                             solver = sl { stage = Interpolate }}
  runReaderT m ps

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runCT :: Model a -> Double -> Solver -> IO [a]
runCT m t sl = do
  d <- runReaderT m $ Parameters { interval = Interval 0 t,
                                   time = 0,
                                   iteration = 0,
                                   solver = sl { stage = SolverStage 0}}
  sequence $ subRunCT d t sl

-- | Auxiliary functions to runCT
subRunCT :: CT a -> Double -> Solver -> [IO a]
subRunCT m t sl = do
  let iv = Interval 0 t
      (nl, nu) = iterationBnds iv (dt sl)
      parameterize n =
        let time = iterToTime iv sl n (SolverStage 0)
            solver = sl {stage = SolverStage 0}
        in Parameters { interval = iv,
                        time = time,
                        iteration = n,
                        solver = solver }

      disct = iterToTime iv sl nu (SolverStage 0)
      values = map (runReaderT m . parameterize) [nl .. nu]
  if disct - t < epslon
  then values
  else let ps = Parameters { interval = iv,
                             time = t,
                             iteration = nu,
                             solver = sl {stage = Interpolate} }
       in init values ++ [runReaderT m ps]
