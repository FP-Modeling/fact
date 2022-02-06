module Main where

import Examples.ChemicalReaction
import Examples.Test
import Simulation.Dynamics

main = 
  do a <- runDynamics model spc
     print a
