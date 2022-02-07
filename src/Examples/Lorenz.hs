module Examples.Lorenz where

import Simulation.Dynamics

lorenzSpecs = Specs { startTime = 0,
                      stopTime = 40,
                      dt = 0.01,
                      method = Euler
                    }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Dynamics (Dynamics [Double])
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
  do ans <- runDynamics1 lorenzModel lorenzSpecs
     print ans
