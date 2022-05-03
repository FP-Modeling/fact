module Main where

import Examples.ChemicalReaction
import Driver
import Dynamics
import IO
import Integrator
import Interpolation
import Memo
import Simulation
import Solver
import Types

main = 
  do a <- runDynamicsFinal model interv solv
     print a
