module Main where

import Examples.ChemicalReaction
import Driver

main = 
  do a <- runDynamicsFinal model interv solv
     print a
