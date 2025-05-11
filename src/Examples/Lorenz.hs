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
-- Module     : Examples.Lorenz
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |

{-# LANGUAGE RecursiveDo #-}
module Examples.Lorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import CT
import Types

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzSolver100 = Solver { dt = 1,
                           method = RungeKutta2,
                           stage = SolverStage 0
                         }

lorenzSolver1k = Solver { dt = 0.1,
                          method = RungeKutta2,
                          stage = SolverStage 0
                        }

lorenzSolver10k = Solver { dt = 0.01,
                           method = RungeKutta2,
                           stage = SolverStage 0
                         }

lorenzSolver100k = Solver { dt = 0.001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }

lorenzSolver1M = Solver { dt = 0.0001,
                          method = RungeKutta2,
                          stage = SolverStage 0
                        }

lorenzSolver10M = Solver { dt = 0.00001,
                           method = RungeKutta2,
                           stage = SolverStage 0
                         }

lorenzSolver100M = Solver { dt = 0.000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }
  

lorenzSolver1B = Solver { dt = 0.0000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }


lorenzSolver10B = Solver { dt = 0.00000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }

                   
lorenzSolver100B = Solver { dt = 0.000000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }

lorenzModel :: Model [Double]
lorenzModel = mdo
   x <- integ (sigma * (y - x)) 1.0
   y <- integ (x * (rho - z) - y) 1.0
   z <- integ (x * y - beta * z) 1.0
   let sigma = 10.0
       rho = 28.0
       beta = 8.0 / 3.0
   return $ sequence [x, y, z]

lorenzSolverYampa = Solver { dt = 0.01,
                           method = Euler,
                           stage = SolverStage 0
                         }

lorenzYampa = runCTFinal lorenzModel 1000 lorenzSolverYampa

lorenz100 = runCTFinal lorenzModel 100 lorenzSolver100

lorenz1k = runCTFinal lorenzModel 100 lorenzSolver1k

lorenz10k = runCTFinal lorenzModel 100 lorenzSolver10k

lorenz100k = runCTFinal lorenzModel 100 lorenzSolver100k

lorenz1M = runCTFinal lorenzModel 100 lorenzSolver1M

lorenz10M = runCTFinal lorenzModel 100 lorenzSolver10M

lorenz100M = runCTFinal lorenzModel 100 lorenzSolver100M

lorenz1B = runCTFinal lorenzModel 100 lorenzSolver1B

lorenz10B = runCTFinal lorenzModel 100 lorenzSolver10B

lorenz100B = runCTFinal lorenzModel 100 lorenzSolver100B
