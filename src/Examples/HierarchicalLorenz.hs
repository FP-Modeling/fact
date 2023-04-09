module Examples.HierarchicalLorenz where

import Driver
import Solver
import Simulation
import Integrator
import IO
import CT
import Prelude hiding (Real)
import Types

hierarchicalLorenzSolver = Solver { dt = 0.01,
                                    method = RungeKutta2,
                                    stage = SolverStage 0
                                  }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

sineModel =
  do integY <- createInteg 0
     integZ <- createInteg 1
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return y

hierarchicalLorenzModel :: CT (CT Real) -> Model Vector
hierarchicalLorenzModel sine =
  do integX <- createInteg 1.0
     integY <- createInteg 1.0
     integZ <- createInteg 1.0
     let x = readInteg integX
         y = readInteg integY
         z = readInteg integZ
     realSine <- sine
     updateInteg integX (realSine * (y - x))
     updateInteg integY (x * (rho - z) - y)
     updateInteg integZ (x * y - beta * z)
     return $ sequence [x, y, z]

hierarchicalLorenz = runCTFinal (hierarchicalLorenzModel sineModel) 100 hierarchicalLorenzSolver
