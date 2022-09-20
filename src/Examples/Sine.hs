{-# LANGUAGE RecordWildCards #-}
{-# RankNTypes #-}
module Examples.Sine where

import Driver
import Solver
import Simulation
import Integrator
import Types
import IO
import CT
import Prelude hiding (Real)
import Data.List
import Simulation

interv = Interval { startTime = 0, 
                    stopTime = 15 }

solv = Solver { dt = 0.01,
                method = RungeKutta4,
                stage = SolverStage 0 }

model :: Model Vector
model =
  do integY <- createInteg 0
     integZ <- createInteg 1
     let y = readInteg integY
         z = readInteg integZ
         kz = -1
     updateInteg integY z
     updateInteg integZ (kz * y)
     return $ sequence [y, z]

-- distort :: Num a => (a -> a) -> CT Real -> CT Real
-- distort distortion (CT machine)
--   = CT $ \ps -> let t = time ps
--                     Interval{..} = interval ps
--                     i = iteration ps
--                 in machine $ ps {time = distortion t,
--                                  iteration = distortion i,
--                                  interval = Interval (distortion startTime) (distortion stopTime)}
                                   
-- modelDistorted :: Model Vector
-- modelDistorted = 
--   do integY <- createInteg 0
--      integZ <- createInteg 1
--      let y = readInteg integY
--          z = readInteg integZ
--      updateInteg integY (distort (*2) z)
--      updateInteg integZ (distort (*2) (-1 * y))
--      return $ sequence [y, z]

-- allResultsSine = runCT modelDistorted 15 solv

-- runCTResult = Data.List.last <$> allNormalResultsSine

-- runCTFinalResult = runCTFinal model 15 solv

-- allNormalResultsSine = runCT model 15 solv

-- sineInputOutput = addTime allResultsSine interv solv

-- sineNormalInputOutput = addTime allNormalResultsSine interv solv

-- writeSine = do wData <- sineInputOutput
--                nData <- sineNormalInputOutput
--                exportData wData "scripts/SineData.txt"
--                exportData nData "scripts/NormalData.txt"


