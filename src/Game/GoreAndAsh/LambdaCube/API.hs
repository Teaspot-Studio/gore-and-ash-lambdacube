{-|
Module      : Game.GoreAndAsh.LambdaCube.API
Description : Monadic and arrow API for module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.LambdaCube.API(
    MonadLambdaCube(..)
  ) where

import Control.Monad.Trans 

import Game.GoreAndAsh.LambdaCube.Module 

-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class Monad m => MonadLambdaCube m where 
  -- | stab method, temporary
  lambdacubeStab :: m ()

instance {-# OVERLAPPING #-} Monad m => MonadLambdaCube (LambdaCubeT s m) where
  lambdacubeStab = return ()

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadLambdaCube m, MonadTrans mt) => MonadLambdaCube (mt m) where 
  lambdacubeStab = lift lambdacubeStab