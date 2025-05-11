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
-- Module     : Benchmarks
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Benchmarks where

import Examples.ChemicalReaction
import Examples.Lorenz
import Examples.Sine
import Driver
import CT
import IO
import Integrator
import Interpolation
import Memo
import Simulation
import Solver
import Types
import Criterion
import Criterion.Measurement
import Data.Int
import qualified Criterion.Measurement.Types as Criterion.Measurement.Measured

perform :: IO [Double] -> IO (Double, Maybe Int64)
perform test = do
  (performance, _) <- measure (nfIO test) 1
  return (Criterion.Measurement.Measured.measTime performance, Criterion.Measurement.Measured.fromInt $ Criterion.Measurement.Measured.measAllocated performance)

benchmarks :: IO ()
benchmarks = do
  results <- mapM perform [lorenz100, lorenz1k, lorenz10k, lorenz100k, lorenz1M, lorenz10M, lorenz100M]
  print results
