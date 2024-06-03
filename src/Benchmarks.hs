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
import Data.Int
import qualified Criterion.Measurement.Types as Criterion.Measurement.Measured

perform :: IO [Double] -> IO (Double, Maybe Int64)
perform test = do
  (performance, _) <- measure (nfIO test) 10
  return (Criterion.Measurement.Measured.measTime performance, Criterion.Measurement.Measured.fromInt $ Criterion.Measurement.Measured.measAllocated performance)

benchmarks :: IO ()
benchmarks = do
  results <- mapM perform [lorenz100, lorenz1k, lorenz10k, lorenz100k, lorenz1M, lorenz10M, lorenz100M]
  print results
