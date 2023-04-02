{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
module Integrator where

import Data.IORef
import Control.Monad.Trans

import Prelude hiding (Real)
import Types
import CT
import Solver
import Interpolation
import Memo
import Simulation
import Data.STRef
import Control.Monad.ST
import Data.Bifunctor
import GHC.Word (gtWord64, gtWord)
import GHC.ExecutionStack (Location(functionName))
import Data.Array.IO
import Control.Monad
       
-- | The Integrator type represents an integral with caching.
data Integrator = Integrator { initial :: CT Real,   -- ^ The initial value.
                               cache   :: IORef (CT Real),
                               computation  :: IORef (CT Real)
                             }

-- data Tuple s = Tuple
--  { first :: STRef s Int
--  , second :: STRef s Int
--  }

data Integrator2 s =
  Integrator2 {
   initial2 :: CT' Real,
   cache2 :: STRef s (CT' (ST s Real)),
   computation2 :: STRef s (CT' (ST s Real))
  }

-- After two sessions trying to swap IO with ST, we concluded that there are two
-- state machines interacting with other, which seems to indicate that it is impossible
-- to use ST, given its constraints.
-- The first machine are the pointers within the integrator, that allows reading the second
-- machine, which is the table saved in memory.
-- The second machine resides in the memoization function, which is responsable the actual
-- computation of data points in the simulation.
-- Given that mdo removes the first machine, we should try to swap in the mdo version of the code.
-- | The Integrator type represents an integral with caching.
data Integrator' s = Integrator' { initial'  :: CT' Real,   -- ^ The initial value.
                                   cache' :: STRef s (CT' (ST s Real)),
                                   computation' :: STRef s (CT' (ST s Real))
                                 }

type StateIntegrator s = ST s (Integrator' s)

-- createInteg' :: CT' Real -> ST s (StateIntegrator s)
-- createInteg' i =
--   return $ do r1 <- newSTRef $ initialize' i
--               r2 <- newSTRef $ initialize' i
--               let integ = Integrator' i r1 r2
--                   z = CT' $ \ps -> fmap (`apply'` ps) (readSTRef (computation' integ))
--               (y :: ST s (CT' Real)) <- memo' interpolate' z
--               modifySTRef (cache' integ) (const y)
--               return integ

-- createInteg' :: CT Real -> CT Integrator
-- createInteg' i =
--   CT $ \ps1 ->
--      do r1 <- newIORef $ initialize i 
--         r2 <- newIORef $ initialize i 
--         let integ = Integrator { initial = i, 
--                                  cache   = r1,
--                                  computation  = r2 }
--             m ps2 = (`apply` ps2) =<< readIORef (computation integ)
--             sl = solver ps1
--             iv = interval ps1
--             (SolverStage stl, SolverStage stu) = stageBnds sl
--             (nl, nu) = iterationBnds iv (dt sl)
--         arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
--         nref  <- newIORef 0
--         stref <- newIORef 0
--         let r ps3 =
--               do let sl  = solver ps3
--                      iv  = interval ps3
--                      n   = iteration ps3
--                      st  = getSolverStage $ stage sl
--                      stu = getSolverStage $ stageHiBnd sl 
--                      loop n' st' = 
--                        if (n' > n) || ((n' == n) && (st' > st)) 
--                        then 
--                          readArray arr (st, n)
--                        else 
--                          let ps3' = ps3 { time = iterToTime iv sl n' (SolverStage st'),
--                                           iteration = n',
--                                           solver = sl { stage = SolverStage st' }}
--                          in do a <- m ps3'
--                                a `seq` writeArray arr (st', n') a
--                                if st' >= stu 
--                                  then do writeIORef stref 0
--                                          writeIORef nref (n' + 1)
--                                          loop (n' + 1) 0
--                                  else do writeIORef stref (st' + 1)
--                                          loop n' (st' + 1)
--                  n'  <- readIORef nref
--                  st' <- readIORef stref
--                  loop n' st'
--         let y = interpolate $ CT r
--         writeIORef (cache integ) y
--         return integ

createInteg' :: CT' Real -> CT' (StateIntegrator s)
createInteg' i =
  CT' $ \ps1 ->
     do r1 <- newSTRef $ initialize'' i 
        r2 <- newSTRef $ initialize'' i 
        let integ = Integrator' { initial' = i, 
                                  cache'   = r1,
                                  computation'  = r2 }
            m ps2 = join $ (`apply'` ps2) <$> readSTRef (computation' integ)
            sl = solver ps1
            iv = interval ps1
            (SolverStage stl, SolverStage stu) = stageBnds sl
            (nl, nu) = iterationBnds iv (dt sl)
        arr   <- newMemoUArray_' ((stl, nl), (stu, nu))
        nref  <- newSTRef 0
        stref <- newSTRef 0
        let r ps3 =
              do let sl  = solver ps3
                     iv  = interval ps3
                     n   = iteration ps3
                     st  = getSolverStage $ stage sl
                     stu = getSolverStage $ stageHiBnd sl 
                     loop n' st' = 
                       if (n' > n) || ((n' == n) && (st' > st)) 
                       then 
                         readArray arr (st, n)
                       else 
                         let ps3' = ps3 { time = iterToTime iv sl n' (SolverStage st'),
                                          iteration = n',
                                          solver = sl { stage = SolverStage st' }}
                         in do a <- m ps3'
                               a `seq` writeArray arr (st', n') a
                               if st' >= stu 
                                 then do writeSTRef stref 0
                                         writeSTRef nref (n' + 1)
                                         loop (n' + 1) 0
                                 else do writeSTRef stref (st' + 1)
                                         loop n' (st' + 1)
                 n'  <- readSTRef nref
                 st' <- readSTRef stref
                 loop n' st'
        let y = CT' r
        writeSTRef (cache' integ) y
        return integ

--readInteg' :: StateIntegrator s -> CT' (ST s Real)
--readInteg' :: Functor f => f (Integrator' s) -> CT' (f (ST s (ST s Real)))
--readInteg' :: StateIntegrator s -> CT' (ST s (ST s Real))
readInteg' :: StateIntegrator s -> CT' (ST s Real)
readInteg' stInteg =
--  CT' $ \ps -> let func integ = fmap (`apply'` ps) (readSTRef (cache' integ))
--               in (join . join) (func <$> stInteg)
    CT' $ \ps -> (join . join) $ ((fmap (`apply'` ps)) . readSTRef . cache') <$> stInteg



updateInteg :: Integrator -> CT Real -> CT ()
updateInteg integ diff = CT $ const $ writeIORef (computation integ) z
  where i = initial integ
        z = CT $ \ps ->
          let f = solverToFunction (method $ solver ps)
          in
          (\y -> f diff i y ps) =<< readIORef (cache integ)
     
-- integ: ST s (Integrator' s)
-- computation <$> integ: ST s (STRef s (CT' (ST s Real)))
-- writeSTRef . computation <$> integ: ST s (ST s ())

-- CT' (ST s Real)

-- updateInteg' :: StateIntegrator s -> CT' (ST s Real) -> CT' (ST s ())
-- updateInteg' integ diff = CT' . const $ join (((\st -> writeSTRef st z) <$> (computation' <$> integ)))
--   where i = initial' <$> integ
--         z = CT' $ \ps ->
--           let f = solverToFunction' (method $ solver ps)
--               k = join $ readSTRef . cache' <$> integ
--           in (\y -> f diff i y ps) =<< k
  

--readInteg' integ = 
--  CT' $ \ps -> fmap (`apply'` ps) (readSTRef (cache' integ)) <$> integ

-- \ps -> IO Real -> \ps -> IO Integrator
createInteg :: CT Real -> CT Integrator
createInteg i =
  CT $ \ps ->
  do r1 <- newIORef $ initialize i 
     r2 <- newIORef $ initialize i 
     let integ = Integrator { initial = i, 
                              cache   = r1,
                              computation  = r2 }
         z = CT $ \ps ->  (`apply` ps) =<< readIORef (computation integ)
     y <- memo interpolate z `apply` ps
     writeIORef (cache integ) y
     return integ


createInteg2 :: CT' Real -> CT' (ST s (Integrator2 s))
createInteg2 i =
  CT' $ \ps -> do
     r1 <- newSTRef $ pure <$> initialize' i 
     r2 <- newSTRef $ pure <$> initialize' i 
     integ <- pure $ Integrator2 { initial2 = i, 
                                   cache2   = r1,
                                   computation2  = r2 }
     z <- pure . CT' $ \ps ->  (`apply'` ps) =<< readSTRef (computation2 integ)
     y <- memo' interpolate' z `apply'` ps
     writeSTRef (cache2 integ) y
     return integ

  
-- readInteg :: Integrator -> CT Real
-- readInteg integ = 
--   CT $ \ps -> (`apply` ps) =<< readIORef (cache integ)

  
readInteg2 :: ST s (Integrator2 s) -> CT' (ST s Real)
readInteg2 integ = 
  CT' $ \ps -> do
  integrator <- integ
  (`apply'` ps) =<< readSTRef (cache2 integrator)


-- updateInteg :: Integrator -> CT Real -> CT ()
-- updateInteg integ diff = CT $ const $ writeIORef (computation integ) z
--   where i = initial integ
--         z = CT $ \ps ->
--           let f = solverToFunction (method $ solver ps)
--           in
--           (\y -> f diff i y ps) =<< readIORef (cache integ)


updateInteg2 :: ST s (Integrator2 s) -> CT' (ST s Real) -> CT' (ST s ())
updateInteg2 integ diff =
  CT' $ \_ -> do
    integrator <- integ
    let i = initial2 integrator
    z <- pure . CT' $ \ps -> do
          f <- pure $ solverToFunction' (method $ solver ps)
          (\y -> f diff i y ps) =<< readSTRef (cache2 integrator)
    writeSTRef (computation2 integrator) z

-- data Integrator' where
--      Integrator' :: {
--         initial' :: CT' Real,
--         cache' :: STRef s (CT' Real), 
--         computation' :: STRef s (CT' Real)
--      } -> Integrator'

-- getComputation :: forall s. Integrator' -> STRef s (CT' Real)
-- getComputation Integrator'{ computation' = x } = x 

-- getCache :: forall s. Integrator' -> STRef s (CT' Real)
-- getCache Integrator'{ cache' = x } = x

initialize :: CT a -> CT a
initialize (CT m) =
  CT $ \ps ->
  if iteration ps == 0 && getSolverStage (stage $ solver ps) == 0 then
    m ps
  else
    let iv = interval ps
        sl = solver ps
    in m $ ps { time = iterToTime iv sl 0 (SolverStage 0),
                iteration = 0,
                solver = sl { stage = SolverStage 0 }}

initialize' :: CT' a -> CT' a
initialize' (CT' m) =
  CT' $ \ps ->
  if iteration ps == 0 && getSolverStage (stage $ solver ps) == 0 then
    m ps
  else
    let iv = interval ps
        sl = solver ps
    in m $ ps { time = iterToTime iv sl 0 (SolverStage 0),
                iteration = 0,
                solver = sl { stage = SolverStage 0 }}


initialize'' :: CT' a -> CT' (ST s a)
initialize'' (CT' m) =
  CT' $ \ps ->
  if iteration ps == 0 && getSolverStage (stage $ solver ps) == 0 then
    return $ m ps
  else
    let iv = interval ps
        sl = solver ps
    in return $ m $ ps { time = iterToTime iv sl 0 (SolverStage 0),
                         iteration = 0,
                         solver = sl { stage = SolverStage 0 }}

-- updateInteg' :: Integrator' s -> CT' Real -> CT' (ST s ())
-- updateInteg' integ diff = CT' $ const $ writeSTRef (computation' integ) z
--   where i = initial' integ
--         z = CT' $ \ps -> do
--           let f = solverToFunction' (method $ solver ps)
--               k = CT' $ \ps -> fmap (`apply'` ps) (readSTRef (computation' integ))
--               table = createTable k `apply'` ps
--               (u :: CT' Real) = undefined
--           modifySTRef (cache' integ) (const u)
--           (\y -> (f diff i y ps)) =<< ((readSTRef (cache' integ)))
  
readInteg :: Integrator -> CT Real
readInteg integ = 
  CT $ \ps -> (`apply` ps) =<< readIORef (cache integ)
     
solverToFunction Euler = integEuler
solverToFunction RungeKutta2 = integRK2
solverToFunction RungeKutta4 = integRK4

solverToFunction' Euler = integEuler'
solverToFunction' _ = error "Use another method"

integEuler :: CT Real
             -> CT Real 
             -> CT Real 
             -> Parameters -> IO Real
integEuler (CT diff) (CT i) (CT y) ps =
  case iteration ps of
    0 -> 
      i ps
    n -> do 
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) (SolverStage 0)
          psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0} }
      a <- y psy
      b <- diff psy
      let !v = a + dt (solver ps) * b
      return v

integRK2 :: CT Real
           -> CT Real
           -> CT Real
           -> Parameters -> IO Real
integRK2 (CT f) (CT i) (CT y) ps =
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 ->
                         i ps
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl (n - 1) (SolverStage 1)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                         vy <- y psy
                         k1 <- f ps1
                         k2 <- f ps2
                         let !v = vy + dt sl / 2.0 * (k1 + k2)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- y psy
                  k1 <- f ps1
                  let !v = vy + dt sl * k1
                  return v
    _ -> 
      error "Incorrect stage: integRK2"

integRK4 :: CT Real
           -> CT Real
           -> CT Real
           -> Parameters -> IO Real
integRK4 (CT f) (CT i) (CT y) ps =
  case stage (solver ps) of
    SolverStage 0 -> case iteration ps of
                       0 -> 
                         i ps
                       n -> do
                         let iv = interval ps
                             sl = solver ps
                             ty = iterToTime iv sl (n - 1) (SolverStage 0)
                             t1 = ty
                             t2 = iterToTime iv sl  (n - 1) (SolverStage 1)
                             t3 = iterToTime iv sl  (n - 1) (SolverStage 2)
                             t4 = iterToTime iv sl  (n - 1) (SolverStage 3)
                             psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
                             ps1 = psy
                             ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
                             ps3 = ps { time = t3, iteration = n - 1, solver = sl { stage = SolverStage 2 }}
                             ps4 = ps { time = t4, iteration = n - 1, solver = sl { stage = SolverStage 3 }}
                         vy <- y psy
                         k1 <- f ps1
                         k2 <- f ps2
                         k3 <- f ps3
                         k4 <- f ps4
                         let !v = vy + dt sl / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
                         return v
    SolverStage 1 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t1 = ty
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps1 = psy
                  vy <- y psy
                  k1 <- f ps1
                  let !v = vy + dt sl / 2.0 * k1
                  return v
    SolverStage 2 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t2 = iterToTime iv sl n (SolverStage 1)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps2 = ps { time = t2, iteration = n, solver = sl { stage = SolverStage 1 }}
                  vy <- y psy
                  k2 <- f ps2
                  let !v = vy + dt sl / 2.0 * k2
                  return v
    SolverStage 3 -> do
                  let iv = interval ps
                      sl = solver ps
                      n  = iteration ps
                      ty = iterToTime iv sl n (SolverStage 0)
                      t3 = iterToTime iv sl n (SolverStage 2)
                      psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
                      ps3 = ps { time = t3, iteration = n, solver = sl { stage = SolverStage 2 }}
                  vy <- y psy
                  k3 <- f ps3
                  let !v = vy + dt sl * k3
                  return v
    _ -> 
      error "Incorrect stase: integRK4"


-- -- | The Integrator' type represents an integral without caching.
-- data Integrator' = Integrator' { initial'     :: CT Real,
--                                  computation' :: IORef (CT Real)
--                                }

-- createInteg' :: CT Double -> CT Integrator'
-- createInteg' i = 
--   do comp <- liftIO $ newIORef $ initialize i 
--      let integ = Integrator'{ initial'     = i, 
--                               computation' = comp }
--      return integ

-- readInteg' :: Integrator' -> CT Real
-- readInteg' integ = 
--   CT $ \ps ->
--   do (CT m) <- readIORef (computation' integ)
--      m ps
     
-- diffInteg' :: Integrator' -> CT Real -> CT ()
-- diffInteg' integ diff =
--   do let z = CT $ \ps ->
--            do y <- readIORef (computation' integ)
--               let i = initial' integ
--               case method (solver ps) of
--                 Euler -> integEuler diff i y ps
--                 RungeKutta2 -> integRK2 diff i y ps
--                 RungeKutta4 -> integRK4 diff i y ps
--      liftIO $ writeIORef (computation' integ) (interpolate z)


integEuler' :: CT' (ST s Real)
             -> CT' Real
             -> CT' (ST s Real)
             -> Parameters -> ST s Real
integEuler' (CT' diff) i (CT' y) ps =
  case iteration ps of
    0 -> 
      pure $ i `apply'` ps
    n ->
      let iv  = interval ps
          sl  = solver ps
          ty  = iterToTime iv sl (n - 1) (SolverStage 0)
          psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0} }
          a = y psy
          b = diff psy
          addMultiply x y z = x + y * z
--          !v = a + dt (solver ps) * b
          !v = addMultiply <$> a <*> pure (dt (solver ps)) <*> b
      in v

-- integRK2' :: CT' Real
--            -> CT' Real
--            -> CT' Real
--            -> Parameters -> ST s Real
-- integRK2' (CT' f) (CT' i) (CT' y) ps =
--   case stage (solver ps) of
--     SolverStage 0 -> case iteration ps of
--                        0 ->
--                          i ps
--                        n -> do
--                          let iv = interval ps
--                              sl = solver ps
--                              ty = iterToTime iv sl (n - 1) (SolverStage 0)
--                              t1 = ty
--                              t2 = iterToTime iv sl (n - 1) (SolverStage 1)
--                              psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
--                              ps1 = psy
--                              ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
--                          vy <- y psy
--                          k1 <- f ps1
--                          k2 <- f ps2
--                          let !v = vy + dt sl / 2.0 * (k1 + k2)
--                          return v
--     SolverStage 1 -> do
--                   let iv = interval ps
--                       sl = solver ps
--                       n  = iteration ps
--                       ty = iterToTime iv sl n (SolverStage 0)
--                       t1 = ty
--                       psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
--                       ps1 = psy
--                   vy <- y psy
--                   k1 <- f ps1
--                   let !v = vy + dt sl * k1
--                   return v
--     _ -> 
--       error "Incorrect stage: integRK2"

-- integRK4' :: CT' Real
--            -> CT' Real
--            -> CT' Real
--            -> Parameters -> IO Real
-- integRK4' (CT' f) (CT' i) (CT' y) ps =
--   case stage (solver ps) of
--     SolverStage 0 -> case iteration ps of
--                        0 -> 
--                          i ps
--                        n -> do
--                          let iv = interval ps
--                              sl = solver ps
--                              ty = iterToTime iv sl (n - 1) (SolverStage 0)
--                              t1 = ty
--                              t2 = iterToTime iv sl  (n - 1) (SolverStage 1)
--                              t3 = iterToTime iv sl  (n - 1) (SolverStage 2)
--                              t4 = iterToTime iv sl  (n - 1) (SolverStage 3)
--                              psy = ps { time = ty, iteration = n - 1, solver = sl { stage = SolverStage 0 }}
--                              ps1 = psy
--                              ps2 = ps { time = t2, iteration = n - 1, solver = sl { stage = SolverStage 1 }}
--                              ps3 = ps { time = t3, iteration = n - 1, solver = sl { stage = SolverStage 2 }}
--                              ps4 = ps { time = t4, iteration = n - 1, solver = sl { stage = SolverStage 3 }}
--                          vy <- y psy
--                          k1 <- f ps1
--                          k2 <- f ps2
--                          k3 <- f ps3
--                          k4 <- f ps4
--                          let !v = vy + dt sl / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
--                          return v
--     SolverStage 1 -> do
--                   let iv = interval ps
--                       sl = solver ps
--                       n  = iteration ps
--                       ty = iterToTime iv sl n (SolverStage 0)
--                       t1 = ty
--                       psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
--                       ps1 = psy
--                   vy <- y psy
--                   k1 <- f ps1
--                   let !v = vy + dt sl / 2.0 * k1
--                   return v
--     SolverStage 2 -> do
--                   let iv = interval ps
--                       sl = solver ps
--                       n  = iteration ps
--                       ty = iterToTime iv sl n (SolverStage 0)
--                       t2 = iterToTime iv sl n (SolverStage 1)
--                       psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
--                       ps2 = ps { time = t2, iteration = n, solver = sl { stage = SolverStage 1 }}
--                   vy <- y psy
--                   k2 <- f ps2
--                   let !v = vy + dt sl / 2.0 * k2
--                   return v
--     SolverStage 3 -> do
--                   let iv = interval ps
--                       sl = solver ps
--                       n  = iteration ps
--                       ty = iterToTime iv sl n (SolverStage 0)
--                       t3 = iterToTime iv sl n (SolverStage 2)
--                       psy = ps { time = ty, iteration = n, solver = sl { stage = SolverStage 0 }}
--                       ps3 = ps { time = t3, iteration = n, solver = sl { stage = SolverStage 2 }}
--                   vy <- y psy
--                   k3 <- f ps3
--                   let !v = vy + dt sl * k3
--                   return v
--     _ -> 
--       error "Incorrect stase: integRK4"


-- -- | The Integrator' type represents an integral without caching.
-- data Integrator' = Integrator' { initial'     :: CT' Real,
--                                  computation' :: IORef (CT' Real)
--                                }

-- createInteg' :: CT' Double -> CT' Integrator'
-- createInteg' i = 
--   do comp <- liftIO $ newIORef $ initialize i 
--      let integ = Integrator'{ initial'     = i, 
--                               computation' = comp }
--      return integ

-- readInteg' :: Integrator' -> CT' Real
-- readInteg' integ = 
--   CT' $ \ps ->
--   do (CT' m) <- readIORef (computation' integ)
--      m ps
     
-- diffInteg' :: Integrator' -> CT' Real -> CT' ()
-- diffInteg' integ diff =
--   do let z = CT' $ \ps ->
--            do y <- readIORef (computation' integ)
--               let i = initial' integ
--               case method (solver ps) of
--                 Euler -> integEuler' diff i y ps
--                 RungeKutta2 -> integRK2 diff i y ps
--                 RungeKutta4 -> integRK4 diff i y ps
--      liftIO $ writeIORef (computation' integ) (interpolate z)
