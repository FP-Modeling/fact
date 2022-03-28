module Examples.Lorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO

lorenzInterv = Interval { startTime = 0,
                          stopTime = 40 }

lorenzSolver = Solver { dt = 0.01,
                        method = Euler,
                        stage = 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model [Double]
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     diffInteg integX (sigma * (y - x))
     diffInteg integY (x * (rho - z) - y)
     diffInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]

mainLorenz =
  do ans <- runDynamicsFinal lorenzModel lorenzInterv lorenzSolver
     print ans

allResultsLorenz = runDynamics lorenzModel lorenzInterv lorenzSolver

lorenzInputOutput = addTime allResultsLorenz lorenzInterv lorenzSolver

writeLorenz = do wData <- lorenzInputOutput
                 exportData wData "scripts/LorenzData.txt"
