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
-- Module     : Solver
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Solver where

import Types ( Iteration )
import Simulation ( Interval(startTime) )

-- | It defines configurations to use within the solver
data Solver = Solver { dt        :: Double,      -- ^ the integration time step
                       method    :: Method,    -- ^ the integration method
                       stage     :: Stage      -- ^ the current stage
                     } deriving (Eq, Ord, Show)

data Stage = SolverStage Int
           | Interpolate
           deriving (Eq, Ord, Show)

getSolverStage :: Stage -> Int
getSolverStage (SolverStage st) = st
getSolverStage Interpolate = 0

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

-- | Make a list of all possible stages, based on the solver method
stages :: Solver -> [Stage]
stages sl = 
  case method sl of
    Euler -> solverStage [0]
    RungeKutta2 -> solverStage [0, 1]
    RungeKutta4 -> solverStage [0, 1, 2, 3]
  where solverStage list = map SolverStage list

-- | Make a tuple with minimum and maximum boundaries, based on the solver method
stageBnds :: Solver -> (Stage, Stage)
stageBnds sl = 
  case method sl of
    Euler -> solverStage (0, 0)
    RungeKutta2 -> solverStage (0, 1)
    RungeKutta4 -> solverStage (0, 3)
  where solverStage (init, end) = (SolverStage init, SolverStage end)

-- | Auxiliary functions for boundaries of stages
stageLoBnd :: Solver -> Stage
stageLoBnd sc = fst $ stageBnds sc
                  
stageHiBnd :: Solver -> Stage
stageHiBnd sc = snd $ stageBnds sc

-- | Transforms iteration to time
iterToTime :: Interval -> Solver -> Iteration -> Stage -> Double
iterToTime _ _ _ Interpolate = error "Incorrect stage: Interpolate"
iterToTime interv solver n (SolverStage st) =
  if st < 0 then 
    error "Incorrect solver stage in iterToTime"
  else
    (startTime interv) + n' * (dt solver) + delta (method solver) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt solver
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt solver / 2
            delta RungeKutta4 2 = dt solver / 2
            delta RungeKutta4 3 = dt solver
