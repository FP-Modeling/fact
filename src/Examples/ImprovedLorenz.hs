module Examples.ImprovedLorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import Dynamics
import Prelude hiding (Real)
import Types

lorenzInterv = Interval { startTime = 0,
                          stopTime = 10 }


lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model Vector
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

mainLorenzFinal =
  do ans <- runDynamicsFinal lorenzModel lorenzInterv lorenzSolver
     print ans

mainLorenz =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver
     print "Done"

allResultsLorenz = runDynamics lorenzModel lorenzInterv lorenzSolver

lorenzInputOutput = addTime allResultsLorenz lorenzInterv lorenzSolver

writeLorenz = do wData <- lorenzInputOutput
                 exportData wData "scripts/LorenzData.txt"


lorenzInterv1 = Interval { startTime = 0,
                           stopTime = 1
                         }

lorenzInterv2 = Interval { startTime = 0,
                           stopTime = 2
                         }

lorenzInterv3 = Interval { startTime = 0,
                           stopTime = 3
                         }

lorenzInterv4 = Interval { startTime = 0,
                           stopTime = 4
                         }

lorenzInterv5 = Interval { startTime = 0,
                           stopTime = 5
                         }

lorenzInterv6 = Interval { startTime = 0,
                           stopTime = 6
                         }

lorenzInterv7 = Interval { startTime = 0,
                           stopTime = 7
                         }

lorenzInterv8 = Interval { startTime = 0,
                           stopTime = 8
                         }

lorenzSolverTest = Solver { dt = 1,
                             method = RungeKutta2,
                             stage = SolverStage 0
                           }

lorenz1 =
  do ans <- runDynamics lorenzModel lorenzInterv1 lorenzSolverTest
     print "Done"

lorenz2 =
  do ans <- runDynamics lorenzModel lorenzInterv2 lorenzSolverTest
     print "Done"

lorenz3 =
  do ans <- runDynamics lorenzModel lorenzInterv3 lorenzSolverTest
     print "Done"

lorenz4 =
  do ans <- runDynamics lorenzModel lorenzInterv4 lorenzSolverTest
     print "Done"

lorenz5 =
  do ans <- runDynamics lorenzModel lorenzInterv5 lorenzSolverTest
     print "Done"

lorenz6 =
  do ans <- runDynamics lorenzModel lorenzInterv6 lorenzSolverTest
     print "Done"

lorenz7 =
  do ans <- runDynamics lorenzModel lorenzInterv7 lorenzSolverTest
     print "Done"

lorenz8 =
  do ans <- runDynamics lorenzModel lorenzInterv8 lorenzSolverTest
     print "Done"


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

lorenz100 =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver100
     print "Done"

lorenz1k =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver1k
     print "Done"

lorenz10k =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver10k
     print "Done"

lorenz100k =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver100k
     print "Done"

lorenz1M =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver1M
     print "Done"

lorenz10M =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver10M
     print "Done"

lorenz100M =
  do ans <- runDynamics lorenzModel lorenzInterv lorenzSolver100M
     print "Done"
