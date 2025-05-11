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
-- Module     : Simulation
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Simulation where

import Types ( Iteration, TimeStep )

-- | It defines a time interval
data Interval = Interval { startTime :: Double, -- ^ the start time
                           stopTime  :: Double  -- ^ the stop time
                         } deriving (Eq, Ord, Show)

-- | Make a list of all possible iterations from 0
iterations :: Interval -> TimeStep -> [Iteration]
iterations interv dt = [i1 .. i2] where
  i1 = 0
  i2 = round ((stopTime interv - 
               startTime interv) / dt)

-- | Make a tuple with minium and maximum boundaries
iterationBnds :: Interval -> TimeStep -> (Iteration, Iteration)
iterationBnds interv dt = (0, ceiling ((stopTime interv - 
                               startTime interv) / dt))

basicIterationBnds :: Interval -> TimeStep -> (Iteration, Iteration)
basicIterationBnds interv dt = (0, round ((stopTime interv - 
                                    startTime interv) / dt))

-- | Auxiliary functions for boundaries of iterations                   
iterationLoBnd :: Interval -> TimeStep -> Iteration
iterationLoBnd interv dt = fst $ iterationBnds interv dt

iterationHiBnd :: Interval -> TimeStep -> Iteration
iterationHiBnd interv dt = snd $ iterationBnds interv dt                   


