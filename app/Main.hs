module Main where

import Examples.ChemicalReaction
import Driver
import CT
import IO
import Integrator
import Interpolation
import Memo
import Simulation
import Solver
import Types

main = 
  do a <- runCTFinal model interv solv
     print a
