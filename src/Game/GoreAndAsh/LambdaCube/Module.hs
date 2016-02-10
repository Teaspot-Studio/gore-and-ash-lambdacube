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

import LambdaCube.GL as LambdaCubeGL

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

instance (MonadIO m, MonadThrow m, GameModule m s) => GameModule (LambdaCubeT s m) (LambdaCubeState s) where 
  type ModuleState (LambdaCubeT s m) = LambdaCubeState s
  runModule (LambdaCubeT m) s = do
    ((a, s'), nextState) <- runModule (runModuleState m s) (lambdacubeNextState s)
    return (a, s' {
        lambdacubeNextState = nextState 
      })  
    where
    runModuleState m s = flip runStateT s $ do 
      a <- m 
      renderStorages =<< get
      return a

  newModuleState = emptyLambdaCubeState <$> newModuleState
  withModule _ = id
  cleanupModule = freeLambdaCubeState

-- | Render all queued storages
renderStorages :: (MonadIO m, MonadThrow m) => LambdaCubeState s -> m ()
renderStorages s@LambdaCubeState{..} = mapM_ renderStorage lambdacubeRenderOrder
  where
  renderStorage si = case getStorageInternal si s of 
    Nothing -> return ()
    Just storage -> case getRendererInternal (storageScheme si) s of 
      Nothing -> return ()
      Just renderer -> do 
        mres <- liftIO $ LambdaCubeGL.setStorage renderer storage
        case mres of 
          Just er -> throwM $! PipeLineIncompatible si er
          Nothing -> liftIO $ LambdaCubeGL.renderFrame renderer