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
