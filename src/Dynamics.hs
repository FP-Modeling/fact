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

-- |
-- Module     : Dynamics
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 8.10.7
-- |

module Dynamics 
       (Dynamics(..),
        Parameters(..)) where

import Control.Monad
import Control.Monad.Trans

import Prelude hiding (Real)
import Types

import Solver
import Simulation

-- | It defines the simulation time appended with additional information.
data Parameters = Parameters { interval  :: Interval, -- ^ the simulation interval
                               solver    :: Solver,   -- ^ the solver configuration
                               time      :: Real,     -- ^ the current time
                               iteration :: Iteration -- ^ the current iteration
                             } deriving (Eq, Show)

newtype Dynamics a = Dynamics {apply :: Parameters -> IO a}

instance Functor Dynamics where
  fmap f (Dynamics da) = Dynamics $ \ps -> fmap f (da ps)

instance Applicative Dynamics where
  pure a = Dynamics $ const (return a)
  (<*>) = appComposition

appComposition :: Dynamics (a -> b) -> Dynamics a -> Dynamics b
appComposition (Dynamics df) (Dynamics da)
  = Dynamics $ \ps -> df ps >>= \f -> fmap f (da ps)
  
instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD k m

returnD :: a -> Dynamics a
returnD a = Dynamics $ const (return a)

bindD :: (a -> Dynamics b ) -> Dynamics a -> Dynamics b
bindD k (Dynamics m) = 
  Dynamics $ \ps -> m ps >>= \a -> (\(Dynamics m') -> m' ps) $ k a

instance Eq (Dynamics a) where
  x == y = error "<< Can't compare dynamics >>" 

instance Show (Dynamics a) where
  showsPrec _ x = showString "<< Dynamics >>"

unaryOP :: (a -> b) -> Dynamics a -> Dynamics b
unaryOP = fmap

binaryOP :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
binaryOP func da db = fmap func da <*> db
  
instance (Num a) => Num (Dynamics a) where
  x + y = binaryOP (+) x y
  x - y = binaryOP (-) x y
  x * y = binaryOP (*) x y
  negate = unaryOP negate
  abs = unaryOP abs
  signum = unaryOP signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = binaryOP (/) x y
  recip = unaryOP recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
  pi = return pi
  exp = unaryOP exp
  log = unaryOP log
  sqrt = unaryOP sqrt
  x ** y = binaryOP (**) x y
  sin = unaryOP sin
  cos = unaryOP cos
  tan = unaryOP tan
  asin = unaryOP asin
  acos = unaryOP acos
  atan = unaryOP atan
  sinh = unaryOP sinh
  cosh = unaryOP cosh
  tanh = unaryOP tanh
  asinh = unaryOP asinh
  acosh = unaryOP acosh
  atanh = unaryOP atanh

instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m     
