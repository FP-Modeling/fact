module Examples.Sine where

import Driver
import Solver
import Simulation
import Integrator
import Types

interv = Interval { startTime = 0, 
                    stopTime = 15 }

solv = Solver { dt = 0.01,
                method = RungeKutta4,
                stage = SolverStage 0 }

model :: Model Vector
model =
  do integY <- createInteg 0
     integZ <- createInteg 1
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return $ sequence [y, z]
