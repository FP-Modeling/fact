module Examples.Test where

import Simulation.Aivika.Dynamics

testSpecs = Specs { spcStartTime = 0,
                    spcStopTime = 1,
                    spcDT = 0.1,
                    spcMethod = RungeKutta4 }

-- model
testModel =
  do integA <- newInteg 50
     integB <- newInteg 0
     let a = integValue integA
     integDiff integB (a)
     return $ sequence [a]

testModel1 =
  do integA <- newInteg 50
     let a = integValueM integA
     integDiff integA (a)
     return $ sequence [a]

edilModel =
  do integA <- newInteg 50
     let a = integValue integA
     return a

parameterise' sc n = Parameters { parSpecs = sc,
                              parTime = basicTime sc n 0,
                              parIteration = n,
                              parPhase = 0}

t index = map (parameterise' testSpecs) [0 .. 10] !! index                   

justSubRun :: Dynamics a -> Specs -> IO a
justSubRun (Dynamics m) sc =
  do let (nl, nu) = iterationBnds sc
         parameterise n = Parameters { parSpecs = sc,
                                       parTime = basicTime sc n 0,
                                       parIteration = n,
                                       parPhase = 0 }
     (m . parameterise) ([nl .. nu] !! 5)



step :: Dynamics (Dynamics a) -> Parameters -> IO a
step (Dynamics m) ps =
  do d <- m ps
     d `apply` ps

justRun :: Dynamics (Dynamics a) -> Specs -> Int -> IO a
justRun (Dynamics m) sc index =
  do d <- m (map (parameterise' sc) [0 .. 10] !! index)
     justSubRun d sc

