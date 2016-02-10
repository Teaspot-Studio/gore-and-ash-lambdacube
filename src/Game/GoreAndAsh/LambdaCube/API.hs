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

import Control.Monad.State.Strict 
import Control.Monad.Trans 
import LambdaCube.GL as LambdaCubeG

import Game.GoreAndAsh.LambdaCube.Module 
import Game.GoreAndAsh.LambdaCube.State 

-- | Low level monadic API for module.
class MonadIO m => MonadLambdaCube m where 
  -- | Update viewport size for rendering engine
  -- Should be called when window size is changed (or every frame)
  lambdacubeUpdateSize :: Int -> Int -> m ()

instance {-# OVERLAPPING #-} MonadIO m => MonadLambdaCube (LambdaCubeT s m) where
  lambdacubeUpdateSize w h = do 
    s <- get 
    liftIO $ updateStateViewportSize w h s

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadLambdaCube m, MonadTrans mt) => MonadLambdaCube (mt m) where 
  lambdacubeUpdateSize a b = lift $ lambdacubeUpdateSize a b