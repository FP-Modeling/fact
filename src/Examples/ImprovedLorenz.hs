module Examples.ImprovedLorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import CT
import Prelude hiding (Real)
import Types
import Control.Monad.ST

lorenzInterv = Interval { startTime = 0,
                          stopTime = 40 }

testLorenzSolver = Solver { dt = 0.001,
                            method = Euler,
                            stage = SolverStage 0
                          }

lorenzSolver = Solver { dt = 0.01,
                        method = RungeKutta2,
                        stage = SolverStage 0
                      }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

--lorenzModel' :: Model Vector
-- lorenzModel' =
--   do integX <- createInteg' 1.0
--      integY <- createInteg' 1.0
--      integZ <- createInteg' 1.0
--      let x = readInteg' integX
--          y = readInteg' integY
--          z = readInteg' integZ
--      updateInteg' integX (()sigma * (y - x))
--      updateInteg' integY (x * (rho - z) - y)
--      updateInteg' integZ (x * y - beta * z)
--      return $ sequence [x, y, z]

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


lorenzModel :: Model Vector
lorenzModel =
  do integX <- createInteg 1.0
     integY <- createInteg 1.0
     integZ <- createInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     updateInteg integX (sigma * (y - x))
     updateInteg integY (x * (rho - z) - y)
     updateInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]

-- monadLorenzTest =
--   do ans <- runCTFinal lorenzModel lorenzInterv testLorenzSolver
--      print ans

-- mainLorenzFinal =
--   do ans1 <- runCTFinal model 40 lorenzSolver
--      ans2 <- runCTFinal' 40 lorenzSolver
--      print ans1
--      print ans2

-- mainLorenz =
--   do ans <- runCT lorenzModel lorenzInterv lorenzSolver
--      print "Done"

-- allResultsLorenz = runCT lorenzModel lorenzInterv testLorenzSolver

-- lorenzInputOutput = addTime allResultsLorenz lorenzInterv testLorenzSolver

-- writeLorenz = do wData <- lorenzInputOutput
--                  exportData wData "scripts/LorenzData.txt"


-- lorenzInterv1 = Interval { startTime = 0,
--                            stopTime = 1
--                          }

-- lorenzInterv2 = Interval { startTime = 0,
--                            stopTime = 2
--                          }

-- lorenzInterv3 = Interval { startTime = 0,
--                            stopTime = 3
--                          }

-- lorenzInterv4 = Interval { startTime = 0,
--                            stopTime = 4
--                          }

-- lorenzInterv5 = Interval { startTime = 0,
--                            stopTime = 5
--                          }

-- lorenzInterv6 = Interval { startTime = 0,
--                            stopTime = 6
--                          }

-- lorenzInterv7 = Interval { startTime = 0,
--                            stopTime = 7
--                          }

-- lorenzInterv8 = Interval { startTime = 0,
--                            stopTime = 8
--                          }

-- lorenzSolverTest = Solver { dt = 1,
--                              method = RungeKutta2,
--                              stage = SolverStage 0
--                            }

-- lorenz1 =
--   do ans <- runCT lorenzModel lorenzInterv1 lorenzSolverTest
--      print "Done"

-- lorenz2 =
--   do ans <- runCT lorenzModel lorenzInterv2 lorenzSolverTest
--      print "Done"

-- lorenz3 =
--   do ans <- runCT lorenzModel lorenzInterv3 lorenzSolverTest
--      print "Done"

-- lorenz4 =
--   do ans <- runCT lorenzModel lorenzInterv4 lorenzSolverTest
--      print "Done"

-- lorenz5 =
--   do ans <- runCT lorenzModel lorenzInterv5 lorenzSolverTest
--      print "Done"

-- lorenz6 =
--   do ans <- runCT lorenzModel lorenzInterv6 lorenzSolverTest
--      print "Done"

-- lorenz7 =
--   do ans <- runCT lorenzModel lorenzInterv7 lorenzSolverTest
--      print "Done"

-- lorenz8 =
--   do ans <- runCT lorenzModel lorenzInterv8 lorenzSolverTest
--      print "Done"


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


lorenzSolver1B = Solver { dt = 0.0000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }


lorenzSolver10B = Solver { dt = 0.00000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }

                   
lorenzSolver100B = Solver { dt = 0.000000001,
                            method = RungeKutta2,
                            stage = SolverStage 0
                          }

lorenzTime = 100

lorenz100 = runCTFinal lorenzModel lorenzTime lorenzSolver100

lorenz1k = runCTFinal lorenzModel lorenzTime lorenzSolver1k

lorenz10k = runCTFinal lorenzModel lorenzTime lorenzSolver10k

lorenz100k = runCTFinal lorenzModel lorenzTime lorenzSolver100k

lorenz1M = runCTFinal lorenzModel lorenzTime lorenzSolver1M

lorenz10M = runCTFinal lorenzModel lorenzTime lorenzSolver10M

lorenz100M = runCTFinal lorenzModel lorenzTime lorenzSolver100M

lorenz1B = runCTFinal lorenzModel lorenzTime lorenzSolver1B

lorenz10B = runCTFinal lorenzModel lorenzTime lorenzSolver10B

lorenz100B = runCTFinal lorenzModel lorenzTime lorenzSolver100B
