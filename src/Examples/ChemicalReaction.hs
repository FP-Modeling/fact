module Examples.ChemicalReaction where

import Driver
import Solver
import Simulation
import Integrator
import Types

chemicalSolv = Solver { dt = 1,
                method = RungeKutta4,
                stage = SolverStage 0 }

chemicalModel :: Model [Double]
chemicalModel =
  do integA <- createInteg 100
     integB <- createInteg 0
     integC <- createInteg 0
     let a = readInteg integA
         b = readInteg integB
         c = readInteg integC
     let ka = 1
         kb = 1
     updateInteg integA (- ka * a )
     updateInteg integB (ka * a - kb * b)
     updateInteg integC (kb * b)
     return $ sequence [a, b, c]

chemical = runCTFinal chemicalModel 10 chemicalSolv
