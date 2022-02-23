module Examples.Lorenz where

import Driver
import Solver
import Simulation
import Integrator

lorenzInterv = Interval { startTime = 0,
                          stopTime = 10 }

lorenzSolver = Solver { iteration = 0,
                        dt = 1,
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
     let x = integValue integX
         y = integValue integY
         z = integValue integZ
     integDiff integX (sigma * (y - x))
     integDiff integY (x * (rho - z) - y)
     integDiff integZ (x * y - beta * z)
     return $ sequence [x, y, z]

mainLorenz =
  do ans <- runDynamicsFinal lorenzModel lorenzInterv lorenzSolver
     print ans
