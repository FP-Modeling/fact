module Examples.BasicLorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import Dynamics
import Prelude hiding (Real)
import Types


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

basicLorenzSolver = Solver { dt = 1,
                             method = RungeKutta2,
                             stage = 0
                           }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

basicLorenzModel :: BasicModel Vector
basicLorenzModel =
  do integX <- newInteg' 1.0
     integY <- newInteg' 1.0
     integZ <- newInteg' 1.0
     let x = readInteg' integX
         y = readInteg' integY
         z = readInteg' integZ
     diffInteg' integX (sigma * (y - x))
     diffInteg' integY (x * (rho - z) - y)
     diffInteg' integZ (x * y - beta * z)
     sequence [x, y, z]

basicLorenz1 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv1 basicLorenzSolver
     print "Done"

basicLorenz2 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv2 basicLorenzSolver
     print "Done"

basicLorenz3 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv3 basicLorenzSolver
     print "Done"

basicLorenz4 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv4 basicLorenzSolver
     print "Done"

basicLorenz5 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv5 basicLorenzSolver
     print "Done"

basicLorenz6 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv6 basicLorenzSolver
     print "Done"

basicLorenz7 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv7 basicLorenzSolver
     print "Done"

basicLorenz8 =
  do ans <- basicRunDynamics basicLorenzModel lorenzInterv8 basicLorenzSolver
     print "Done"
