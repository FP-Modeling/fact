module Examples.Lorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import Dynamics
import Prelude hiding (Real)
import Types

lorenzInterv = Interval { startTime = 0,
                          stopTime = 40 }


lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = 0
                      }

lorenzSolver2 = Solver { dt = 1,
                        method = RungeKutta2,
                        stage = 0
                       }


lorenzInterv2 = Interval { startTime = 0,
                           stopTime = 5.1 }


lorenzInterv3 = Interval { startTime = 0,
                           stopTime = 6 }


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


lorenzModel' :: Dynamics [Double]
lorenzModel' =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     diffInteg integX (sigma * (y - x))
     diffInteg integY (x * (rho - z) - y)
     diffInteg integZ (x * y - beta * z)
     sequence [x, y, z]

t :: Dynamics Real
t = Dynamics $ \ps -> return (time ps)


exampleInterv = Interval { startTime = 0,
                          stopTime = 5 }

exampleSolver = Solver { dt = 1,
                        method = Euler,
                        stage = 0
                       }

exampleModel :: Model [Real]
exampleModel =
  do integ <- newInteg 1
     let y = readInteg integ
     diffInteg integ (y + t)
     return $ sequence [y]


exampleModel2 :: Dynamics [Dynamics Real]
exampleModel2 =
  do integX <- newInteg 1
     integY <- newInteg 1
     let x = readInteg integX
         y = readInteg integY
     diffInteg integX (x * y)
     diffInteg integY (y + t)
     return [x, y]


type Vector = [Double]

exampleModel3 :: Model2 Vector
exampleModel3 =
  do integX <- newInteg 1
     integY <- newInteg 1
     let x = readInteg integX
         y = readInteg integY
     diffInteg integX (x * y)
     diffInteg integY (y + t)
     sequence [x, y]

exampleModel4 :: Model Vector
exampleModel4 =
  do integX <- newInteg 1
     integY <- newInteg 1
     let x = readInteg integX
         y = readInteg integY
     diffInteg integX (x * y)
     diffInteg integY (y + t)
     return $ sequence [x, y]

mainLorenz =
  do ans <- runDynamicsFinal lorenzModel lorenzInterv2 lorenzSolver2
     print ans

mainLorenz2 =
  do ans <- runDynamics lorenzModel lorenzInterv2 lorenzSolver2
     print ans

mainLorenz3 =
  do ans <- runDynamics lorenzModel lorenzInterv3 lorenzSolver2
     print ans

exampleTest3 = runDynamicsTest exampleModel3 exampleInterv exampleSolver

exampleTest4 = runDynamics exampleModel4 exampleInterv exampleSolver

allResultsLorenz = runDynamics lorenzModel lorenzInterv lorenzSolver

lorenzInputOutput = addTime allResultsLorenz lorenzInterv lorenzSolver

writeLorenz = do wData <- lorenzInputOutput
                 exportData wData "scripts/LorenzData.txt"
