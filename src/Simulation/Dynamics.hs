
-- Copyright (c) 2009, 2010, 2011 David Sorokin <david.sorokin@gmail.com>
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, BangPatterns, ConstraintKinds, MonoLocalBinds #-}

-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 6.12.1
--
-- Aivika is a multi-paradigm simulation library. It allows us to integrate 
-- a system of ordinary differential equations. Also it can be applied to
-- the Discrete Event Simulation. It supports the event-oriented, 
-- process-oriented and activity-oriented paradigms. Aivika also supports 
-- the Agent-based Modeling. Finally, it can be applied to System Dynamics.
--
module Simulation.Dynamics 
       (-- * Dynamics
        Dynamics(..),
        iterationBnds,
        iterToTime,
        Specs(..),
        Method(..),
        Parameters(..),
        runDynamics1,
        runDynamics,
        -- ** Integrals
        Integ,
        newInteg,
        initial,
        integValue,
        integDiff,
        -- ** Table Functions
        lookupD,
        lookupStepwiseD,
        -- ** Interpolation
        initD,
        discrete,
        interpolate,
        -- ** Memoization and Sequential Calculations
        Memo,
        UMemo,
        memo,
        umemo,
        memo0,
        umemo0,
        -- ** Utility
        once) where

import Data.Array
import Data.Array.IO
import Data.IORef
import Control.Monad
import Control.Monad.Trans

--
-- The Dynamics Monad
--
-- A value of the Dynamics monad represents an abstract dynamic 
-- process, i.e. a time varying polymorstic function. This is 
-- a key point of the Aivika simulation library.
--

-- | A value in the 'Dynamics' monad represents a dynamic process, i.e.
-- a polymorstic time varying function.
newtype Dynamics a = Dynamics {apply :: Parameters -> IO a}

-- | It defines the simulation time appended with additional information.
data Parameters = Parameters { specs :: Specs,    -- ^ the simulation specs
                               time :: Double,    -- ^ the current time
                               iteration :: Int,  -- ^ the current iteration
                               stage :: Int }     -- ^ the current stage
                  deriving (Eq, Show)

-- | It defines the simulation specs.
data Specs = Specs { startTime :: Double,    -- ^ the start time
                     stopTime  :: Double,    -- ^ the stop time
                     dt        :: Double,    -- ^ the integration time step
                     method :: Method        -- ^ the integration method
                   } deriving (Eq, Ord, Show)

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

instance Functor Dynamics where
  fmap f (Dynamics da) = Dynamics $ \ps -> fmap f (da ps)

instance Applicative Dynamics where
  pure = returnD
  (Dynamics df) <*> (Dynamics da) = Dynamics $ \ps -> flip fmap (da ps) =<< df ps
  
instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD m k

returnD :: a -> Dynamics a
returnD a = Dynamics $ const (return a)

bindD :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
bindD (Dynamics m) k = 
  Dynamics $ \ps -> 
  do a <- m ps
     let Dynamics m' = k a
     m' ps


instance Eq (Dynamics a) where
  x == y = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ x = showString "<< Dynamics >>"

liftMD :: (a -> b) -> Dynamics a -> Dynamics b
liftMD f (Dynamics x) =
  Dynamics $ \ps -> do { a <- x ps; return $ f a }

liftM2D :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \ps -> do { a <- x ps; b <- y ps; return $ f a b }

instance (Num a) => Num (Dynamics a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
  pi = return pi
  exp = liftMD exp
  log = liftMD log
  sqrt = liftMD sqrt
  x ** y = liftM2D (**) x y
  sin = liftMD sin
  cos = liftMD cos
  tan = liftMD tan
  asin = liftMD asin
  acos = liftMD acos
  atan = liftMD atan
  sinh = liftMD sinh
  cosh = liftMD cosh
  tanh = liftMD tanh
  asinh = liftMD asinh
  acosh = liftMD acosh
  atanh = liftMD atanh

instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m
     

-- | Make a list of all possible iterations from 0
iterations :: Specs -> [Int]
iterations sc = [i1 .. i2] where
  i1 = 0
  i2 = round ((stopTime sc - 
               startTime sc) / dt sc)

-- | Make a tuple with minium and maximum boundaries
iterationBnds :: Specs -> (Int, Int)
iterationBnds sc = (0, round ((stopTime sc - 
                               startTime sc) / dt sc))

-- | Auxiliary functions for boundaries of iterations                   
iterationLoBnd :: Specs -> Int
iterationLoBnd sc = fst $ iterationBnds sc

iterationHiBnd :: Specs -> Int
iterationHiBnd sc = snd $ iterationBnds sc                   

-- | Make a list of all possible stages, based on the solver method
stages :: Specs -> [Int]
stages sc = 
  case method sc of
    Euler -> [0]
    RungeKutta2 -> [0, 1]
    RungeKutta4 -> [0, 1, 2, 3]

-- | Make a tuple with minimum and maximum boundaries, based on the solver method
stageBnds :: Specs -> (Int, Int)
stageBnds sc = 
  case method sc of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

-- | Auxiliary functions for boundaries of stages
stageLoBnd :: Specs -> Int
stageLoBnd sc = fst $ stageBnds sc
                  
stageHiBnd :: Specs -> Int
stageHiBnd sc = snd $ stageBnds sc

-- | Transforms iteration to time
iterToTime :: Specs -> Int -> Int -> Double
iterToTime sc n st =
  if st < 0 then 
    error "Incorrect stage: iterToTime"
  else
    (startTime sc) + n' * (dt sc) + delta (method sc) st
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = dt sc
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = dt sc / 2
            delta RungeKutta4 2 = dt sc / 2
            delta RungeKutta4 3 = dt sc

-- | Function to solve floating point approximations
neighborhood :: Specs -> Double -> Double -> Bool
neighborhood sc t t' = 
  abs (t - t') <= dt sc / 1.0e6

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runDynamics1 :: Dynamics (Dynamics a) -> Specs -> IO a
runDynamics1 (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     subrunDynamics1 d sc

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamics :: Dynamics (Dynamics a) -> Specs -> IO [a]
runDynamics (Dynamics m) sc = 
  do d <- m Parameters { specs = sc,
                         time = startTime sc,
                         iteration = 0,
                         stage = 0 }
     sequence $ subrunDynamics d sc

-- | Auxiliary functions to runDyanamics (individual computation and list of computations)
subrunDynamics1 :: Dynamics a -> Specs -> IO a
subrunDynamics1 (Dynamics m) sc =
  do let n = iterationHiBnd sc
         t = iterToTime sc n 0
     m Parameters { specs = sc,
                    time = t,
                    iteration = n,
                    stage = 0 }

subrunDynamics :: Dynamics a -> Specs -> [IO a]
subrunDynamics (Dynamics m) sc =
  do let (nl, nu) = iterationBnds sc
         parameterise n = Parameters { specs = sc,
                                       time = iterToTime sc n 0,
                                       iteration = n,
                                       stage = 0 }
     map (m . parameterise) [nl .. nu]
           
--
-- Integral type
--

-- | The 'Integ' type represents an integral.
data Integ = Integ { initial     :: Dynamics Double,   -- ^ The initial value.
                     cache :: IORef (Dynamics Double),
                     result :: IORef (Dynamics Double) }

-- | Create a new integral with the specified initial value.
newInteg :: Dynamics Double -> Dynamics Integ
newInteg i = 
  do r1 <- liftIO $ newIORef $ initD i 
     r2 <- liftIO $ newIORef $ initD i 
     let integ = Integ { initial = i, 
                         cache   = r1,
                         result  = r2 }
         z = Dynamics $ \ps -> 
           do (Dynamics m) <- readIORef (result integ)
              m ps
     y <- umemo interpolate z
     liftIO $ writeIORef (cache integ) y
     return integ

     -- Me de qualquer coisa que te dou 50
     -- Faça esta conta

-- | Return the integral's value.
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (cache integ)
     m ps

-- | Set the derivative for the integral.
integDiff :: Integ -> Dynamics Double -> Dynamics ()
integDiff integ diff =
  do let z = Dynamics $ \ps ->
           do y <- readIORef (cache integ) -- Give me past values
              let i = initial integ -- Give me initial value
              case method (specs ps) of -- Check the solver method
                Euler -> integEuler diff i y ps
                RungeKutta2 -> integRK2 diff i y ps
                RungeKutta4 -> integRK4 diff i y ps
     liftIO $ writeIORef (result integ) z -- This is the new computation now!

integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Parameters -> IO Double
integEuler (Dynamics diff) (Dynamics i) (Dynamics y) ps =
  case iteration ps of
    0 -> 
      i ps
    n -> do 
      let sc  = specs ps
          ty  = iterToTime sc (n - 1) 0
          psy = ps { time = ty, iteration = n - 1, stage = 0 }
      a <- y psy
      b <- diff psy
      let !v = a + dt (specs ps) * b
      return v

integRK2 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK2 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage ps of
    0 -> case iteration ps of
      0 ->
        i ps
      n -> do
        let sc = specs ps
            ty = iterToTime sc (n - 1) 0
            t1 = ty
            t2 = iterToTime sc (n - 1) 1
            psy = ps { time = ty, iteration = n - 1, stage = 0 }
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, stage = 1 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        let !v = vy + dt sc / 2.0 * (k1 + k2)
        return v
    1 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t1 = ty
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sc * k1
      return v
    _ -> 
      error "Incorrect stase: integ"

integRK4 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK4 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case stage ps of
    0 -> case iteration ps of
      0 -> 
        i ps
      n -> do
        let sc = specs ps
            ty = iterToTime sc (n - 1) 0
            t1 = ty
            t2 = iterToTime sc (n - 1) 1
            t3 = iterToTime sc (n - 1) 2
            t4 = iterToTime sc (n - 1) 3
            psy = ps { time = ty, iteration = n - 1, stage = 0 }
            ps1 = psy
            ps2 = ps { time = t2, iteration = n - 1, stage = 1 }
            ps3 = ps { time = t3, iteration = n - 1, stage = 2 }
            ps4 = ps { time = t4, iteration = n - 1, stage = 3 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        k3 <- f ps3
        k4 <- f ps4
        let !v = vy + dt sc / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        return v
    1 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t1 = ty
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + dt sc / 2.0 * k1
      return v
    2 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t2 = iterToTime sc n 1
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps2 = ps { time = t2, iteration = n, stage = 1 }
      vy <- y psy
      k2 <- f ps2
      let !v = vy + dt sc / 2.0 * k2
      return v
    3 -> do
      let sc = specs ps
          n  = iteration ps
          ty = iterToTime sc n 0
          t3 = iterToTime sc n 2
          psy = ps { time = ty, iteration = n, stage = 0 }
          ps3 = ps { time = t3, iteration = n, stage = 2 }
      vy <- y psy
      k3 <- f ps3
      let !v = vy + dt sc * k3
      return v
    _ -> 
      error "Incorrect stase: integ"

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
lookupD :: Dynamics Double -> Array Int (Double, Double) -> Dynamics Double
lookupD (Dynamics m) tbl =
  Dynamics (\ps -> do a <- m ps; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = 
                     let x2 = fst $ tbl ! (index + 1)
                         y1 = snd $ tbl ! index
                         y2 = snd $ tbl ! (index + 1)
                     in y1 + (y2 - y1) * (x - x1) / (x2 - x1) 
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
lookupStepwiseD :: Dynamics Double -> Array Int (Double, Double)
                  -> Dynamics Double
lookupStepwiseD (Dynamics m) tbl =
  Dynamics (\ps -> do a <- m ps; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = snd $ tbl ! right
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y


-- | Return the initial value.
initD :: Dynamics a -> Dynamics a
initD (Dynamics m) =
  Dynamics $ \ps ->
  if iteration ps == 0 && stage ps == 0 then
    m ps
  else
    let sc = specs ps
    in m $ ps { time = iterToTime sc 0 0,
                iteration = 0,
                stage = 0 } 

-- | Discretize the computation in the integration time points.
discrete :: Dynamics a -> Dynamics a
discrete (Dynamics m) =
  Dynamics $ \ps ->
  let st = stage ps
      r | st == 0    = m ps
        | st > 0    = let sc = specs ps
                          n  = iteration ps
                      in m $ ps { time = iterToTime sc n 0,
                                  stage = 0 }
        | otherwise = let sc = specs ps
                          t  = time ps
                          n  = iteration ps
                          t' = startTime sc + fromIntegral (n + 1) * dt sc
                          n' = if neighborhood sc t t' then n + 1 else n
                      in m $ ps { time = iterToTime sc n' 0,
                                  iteration = n',
                                  stage = 0 }
  in r

-- | Interpolate the computation based on the integration time points only.
interpolate :: Dynamics Double -> Dynamics Double
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if stage ps >= 0 then 
    m ps
  else 
    let sc = specs ps
        t  = time ps
        x  = (t - startTime sc) / dt sc
        n1 = max (floor x) (iterationLoBnd sc)
        n2 = min (ceiling x) (iterationHiBnd sc)
        t1 = iterToTime sc n1 0
        t2 = iterToTime sc n2 0
        z1 = m $ ps { time = t1, 
                      iteration = n1, 
                      stage = 0 }
        z2 = m $ ps { time = t2,
                      iteration = n2,
                      stage = 0 }
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r

-- --
-- -- Memoization
-- --
-- -- The memoization creates such processes, which values are 
-- -- defined and then stored in the cache for the points of
-- -- integration. You should use some kind of interpolation 
-- -- like the interpolate function to process all other time 
-- -- points that don't coincide with the integration points:
-- --
-- --   x = memo interpolate y    -- a linear interpolation
-- --   x = memo discrete y       -- a discrete process
-- --

-- -- | The 'Memo' class specifies a type for which an array can be created.
class (MArray IOArray e IO) => Memo e where
  newMemoArray_ :: Ix i => (i, i) -> IO (IOArray i e)

-- -- | The 'UMemo' class specifies a type for which an unboxed array exists.
class (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance Memo e where
  newMemoArray_ = newArray_
    
instance (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ = newArray_

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
memo :: Memo e => (Dynamics e -> Dynamics e) -> Dynamics e 
       -> Dynamics (Dynamics e)
memo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc = specs ps
         (stl, stu) = stageBnds sc
         (nl, nu)   = iterationBnds sc
     arr   <- newMemoArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps = 
           do let sc  = specs ps
                  n   = iteration ps
                  st  = stage ps
                  stu = stageHiBnd sc 
                  loop n' st' = 
                    if (n' > n) || ((n' == n) && (st' > st)) 
                    then 
                      readArray arr (st, n)
                    else 
                      let ps' = ps { iteration = n', stage = st',
                                     time = iterToTime sc n' st' }
                      in do a <- m ps'
                            a `seq` writeArray arr (st', n') a
                            if st' >= stu 
                              then do writeIORef stref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef stref (st' + 1)
                                      loop n' (st' + 1)
              n'  <- readIORef nref
              st' <- readIORef stref
              loop n' st'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
umemo :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
umemo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc = specs ps
         (stl, stu) = stageBnds sc
         (nl, nu)   = iterationBnds sc
     arr   <- newMemoUArray_ ((stl, nl), (stu, nu))
     nref  <- newIORef 0
     stref <- newIORef 0
     let r ps =
           do let sc  = specs ps
                  n   = iteration ps
                  st  = stage ps
                  stu = stageHiBnd sc 
                  loop n' st' = 
                    if (n' > n) || ((n' == n) && (st' > st)) 
                    then 
                      readArray arr (st, n)
                    else 
                      let ps' = ps { iteration = n', 
                                     stage = st',
                                     time = iterToTime sc n' st' }
                      in do a <- m ps'
                            a `seq` writeArray arr (st', n') a
                            if st' >= stu 
                              then do writeIORef stref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef stref (st' + 1)
                                      loop n' (st' + 1)
              n'  <- readIORef nref
              st' <- readIORef stref
              loop n' st'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
memo0 tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc   = specs ps
         bnds = iterationBnds sc
     arr   <- newMemoArray_ bnds
     nref  <- newIORef 0
     let r ps =
           do let sc = specs ps
                  n  = iteration ps
                  loop n' = 
                    if n' > n
                    then 
                      readArray arr n
                    else 
                      let ps' = ps { iteration = n', stage = 0,
                                     time = iterToTime sc n' 0 }
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
umemo0 :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
         -> Dynamics (Dynamics e)
umemo0 tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc   = specs ps
         bnds = iterationBnds sc
     arr   <- newMemoUArray_ bnds
     nref  <- newIORef 0
     let r ps =
           do let sc = specs ps
                  n  = iteration ps
                  loop n' = 
                    if n' > n
                    then 
                      readArray arr n
                    else 
                      let ps' = ps { iteration = n', stage = 0,
                                     time = iterToTime sc n' 0 }
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ Dynamics r

--
-- Once
--

-- | Call the computation only once.
once :: Dynamics a -> Dynamics (Dynamics a)
once (Dynamics m) =
  Dynamics $ \ps ->
  do x <- newIORef Nothing
     let r ps =
           do a <- readIORef x
              case a of
                Just b -> 
                  return b
                Nothing ->
                  do b <- m ps
                     writeIORef x $ Just b
                     return $! b
     return $ Dynamics r          
