module Benchmarks where

import Examples.ChemicalReaction
import Examples.Lorenz
import Driver
import CT
import IO
import Integrator
import Interpolation
import Memo
import Simulation
import Solver
import Types
import Criterion
import Criterion.Measurement
import qualified Criterion.Types as Criterion
import qualified Criterion.Measurement.Types as Criterion.Measurement
import qualified Criterion.Measurement.Types as Criterion.Measurement.Measured

perform test = do
  (performance, result) <- measure (nfIO test) 1
  return (Criterion.Measurement.Measured.measTime performance, result)

benchmarks :: IO ()
benchmarks = do
  results <- sequence $ perform <$> [lorenz100, lorenz1k, lorenz10k, lorenz100k, lorenz1M, lorenz10M, lorenz100M]
  print results
