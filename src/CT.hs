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
-- Module     : CT
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : Eduardo Lemos Rocha <dudulr10@gmail.com>
-- Stability  : stable
-- Tested with: GHC 8.10.7
-- |
{-# LANGUAGE FlexibleInstances #-}
module CT 
       (CT(..),
        Parameters(..)) where

import Control.Monad.Trans.Reader ( ReaderT )

import Types ( Iteration )

import Solver ( Solver )
import Simulation ( Interval )

-- | It defines the simulation time appended with additional information.
data Parameters = Parameters { interval  :: Interval, -- ^ the simulation interval
                               solver    :: Solver,   -- ^ the solver configuration
                               time      :: Double,     -- ^ the current time
                               iteration :: Iteration -- ^ the current iteration
                             } deriving (Eq, Show)


type CT a = ReaderT Parameters IO a
  
instance (Num a) => Num (CT a) where
  x + y = (+) <$> x <*> y
  x - y = (-) <$> x <*> y
  x * y = (*) <$> x <*> y
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (CT a) where
  x / y = (/) <$> x <*> y
  recip = fmap recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (CT a) where
  pi = return pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  x ** y = (**) <$> x <*> y
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
