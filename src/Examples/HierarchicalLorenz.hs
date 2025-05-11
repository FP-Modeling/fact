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
-- Module     : Examples.HierarchicalLorenz
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |
module Examples.HierarchicalLorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import CT
import Types

hierarchicalLorenzSolver = Solver { dt = 0.01,
                                    method = RungeKutta2,
                                    stage = SolverStage 0
                                  }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

sineModel =
  do integY <- createInteg 0
     integZ <- createInteg 1
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return y

hierarchicalLorenzModel :: CT (CT Double) -> Model [Double]
hierarchicalLorenzModel sine =
  do integX <- createInteg 1.0
     integY <- createInteg 1.0
     integZ <- createInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     realSine <- sine
     updateInteg integX (realSine * (y - x))
     updateInteg integY (x * (rho - z) - y)
     updateInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]

hierarchicalLorenz = runCTFinal (hierarchicalLorenzModel sineModel) 100 hierarchicalLorenzSolver
