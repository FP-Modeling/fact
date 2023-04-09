module Main where

import Examples.ChemicalReaction

main :: IO ()
main = chemical >>= print
