{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.LambdaCube.Module
Description : Monad transformer and instance for core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.LambdaCube.Module(
    LambdaCubeT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube.State

-- | Monad transformer of the core module.
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
-- 
-- @
-- type AppStack = ModuleStack [LambdaCubeT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadLambdaCube)
-- @
--
-- The module is not pure within first phase (see 'ModuleStack' docs), therefore only 'IO' can be used as end monad.
newtype LambdaCubeT s m a = LambdaCubeT { runLambdaCubeT :: StateT (LambdaCubeState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (LambdaCubeState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (LambdaCubeT s m) (LambdaCubeState s) where 
  type ModuleState (LambdaCubeT s m) = LambdaCubeState s
  runModule (LambdaCubeT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (lambdacubeNextState s)
    return (a, s' {
        lambdacubeNextState = nextState 
      })  
  
  newModuleState = emptyLambdaCubeState <$> newModuleState
  withModule _ = id
  cleanupModule = freeLambdaCubeState
