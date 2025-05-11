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
-- Module     : Examples.Test
-- Copyright  : Copyright (c) 2025, Eduardo Lemos <dudulr10@gmail.com>, Edil Medeiros <j.edil@ene.unb.br>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 9.6.6
-- |

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Examples.Test where

import Driver
import Solver
import Integrator
import Types
import IO
import CT
import Data.List
import Simulation
import Control.Monad.ListM
import GHC.Base (MonadPlus)
import Examples.Sine (sineSolv)

sineSolv2 = Solver { dt = 1,
                    method = RungeKutta4,
                    stage = SolverStage 0 }

type Result = (Double, String)

sineModel2 :: CT Double -> Model [Double]
sineModel2 ic =
  do integY <- createInteg 0
     integZ <- createInteg ic
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return $ sequence [y]

example1 :: CT Double -> Model [Result]
example1 ic =
  do integY <- createInteg ic
     let y = readInteg integY
     updateInteg integY 1
     return $ map (, "line")<$> sequence [y]

example2 :: CT Double -> Model [Result]
example2 ic =
  do integY <- createInteg ic
     let y = readInteg integY
     updateInteg integY y
     return $ map (, "parabole")<$> sequence [y]

-- sine = runCTFinal sineModel 15 sineSolv

parabole = runCT (example2 20) 10 sineSolv2

line = runCT (example1 1) 40 sineSolv2

type HybridModel a = a -> Parameters -> IO a

type Predicate a b = a -> (CT Double -> Model [b])

predicate :: (Ord a, Num a) => a -> (CT Double -> Model [Result])
predicate initialCondition =
  if initialCondition >= 20
  then example2 else example1

-- demux :: Predicate Double Result -> HybridModel Result
-- demux predicate (initialCondition, _) p = do
--   let m = predicate initialCondition
--   model <- m (pure initialCondition) `apply` p
--   head <$> model `apply` p
  
-- hybrid :: (MonadPlus p, Monad m) => (a -> Parameters -> m a) -> a -> Double -> Solver -> m (p a)
-- hybrid f z t sl =
--   do let iv = Interval 0 t
--          (nl, nu) = iterationBnds iv (dt sl)
--          parameterise n = Parameters { interval = iv,
--                                        time = iterToTime iv sl n (SolverStage 0),
--                                        iteration = 1,
--                                        solver = sl { stage = SolverStage 0 }}
--          ps = map parameterise [nl..nu]
--      scanM f z ps
        
-- test = do
--   t <- hybrid (demux predicate) (1, "initial") 40 sineSolv2
--   case t of
--     [] -> fail "Something went wrong during hybrid simulation"
--     list -> print list
