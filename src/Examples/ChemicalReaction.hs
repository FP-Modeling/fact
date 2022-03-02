module Examples.ChemicalReaction where

import Driver
import Solver
import Simulation
import Integrator

interv = Interval { startTime = 0, 
                    stopTime = 10 }

solv = Solver { dt = 1,
                method = RungeKutta4,
                stage = 0 }

model :: Model [Double]
model =
  do integA <- newInteg 100
     integB <- newInteg 0
     integC <- newInteg 0
     let a = integValue integA
         b = integValue integB
         c = integValue integC
     let ka = 1
         kb = 1
     integDiff integA (- ka * a )
     integDiff integB (ka * a - kb * b)
     integDiff integC (kb * b)
     return $ sequence [a, b, c]
