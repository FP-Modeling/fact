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
-- Module     : Interpolation
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Interpolation where

import CT ( Parameters(solver, interval, time, iteration), CT )
import Simulation
    ( Interval(startTime), iterationLoBnd, iterationHiBnd )
import Solver
    ( Solver(stage, dt),
      Stage(SolverStage, Interpolate),
      getSolverStage,
      iterToTime )
import Control.Monad.Trans.Reader

-- | Function to solve floating point approximations
neighborhood :: Solver -> Double -> Double -> Bool
neighborhood sl t t' = 
  abs (t - t') <= dt sl / 1.0e6

-- | Interpolate the computation based on the integration time points only.
interpolate :: CT Double -> CT Double
interpolate m = do
  ps <- ask
  case stage $ solver ps of
    SolverStage _ -> m
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
          ps1 = ps { time = t1,
                     iteration = n1,
                     solver = sl { stage = SolverStage 0 }}
          ps2 = ps { time = t2,
                     iteration = n2,
                     solver = sl { stage = SolverStage 0 }}
          z1 = local (const ps1) m
          z2 = local (const ps2) m
      in z1 + (z2 - z1) * pure ((t - t1) / (t2 - t1))
