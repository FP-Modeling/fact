module Main where

import Examples.ChemicalReaction
import Simulation.Aivika.Dynamics

main = 
  do a <- runDynamics model specs
     print a
