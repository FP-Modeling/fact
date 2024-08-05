module Examples.Sine where

import Driver
import Solver
import Integrator
import Types
import IO
import CT
import Data.List
import Simulation

sineSolv = Solver { dt = 0.01,
                    method = RungeKutta4,
                    stage = SolverStage 0 }

sineModel :: Model [Double]
sineModel =
  do integY <- createInteg 0
     integZ <- createInteg 1
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return $ sequence [y, z]

sine = runCTFinal sineModel 15 sineSolv
     
