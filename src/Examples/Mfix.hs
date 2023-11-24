{-# LANGUAGE RecursiveDo #-}
module Examples.Mfix where

import Driver
import Solver
import Integrator
import Types
import IO
import CT
import Prelude hiding (Real)
import Data.List
import Simulation


mchemicalSolv = Solver { dt = 1,
                method = RungeKutta4,
                stage = SolverStage 0 }

mchemicalModel :: Model Vector
mchemicalModel = 
  mdo a <- integ (- ka * a) 100
      b <- integ (ka * a - kb * b) 0
      c <- integ (kb * b) 0
      let ka = 1
          kb = 1
      return $ sequence [a, b, c]

mchemical = runCTFinal mchemicalModel 10 mchemicalSolv
