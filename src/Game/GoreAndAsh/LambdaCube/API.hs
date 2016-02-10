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
  , LambdaCubeException(..)
  ) where

import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.State.Strict 
import Control.Monad.Writer (Writer)

import LambdaCube.Compiler as LambdaCube
import LambdaCube.GL as LambdaCubeGL

import Game.GoreAndAsh.LambdaCube.Module 
import Game.GoreAndAsh.LambdaCube.State 

-- | Low level monadic API for module.
class (MonadIO m, MonadThrow m) => MonadLambdaCube m where 
  -- | Update viewport size for rendering engine
  -- Should be called when window size is changed (or every frame)
  lambdacubeUpdateSize :: Word -- ^ Width of screen in pixels
    -> Word -- ^ Height of screen in pixels 
    -> m ()

  -- | Compile and register new pipeline.
  --
  -- Throws: 'PipeLineCompileFailed' or 'PipeLineAlreadyRegistered' when failed.
  lambdacubeAddPipeline :: 
    [FilePath] -- ^ Where to find LC modules
    -> String -- ^ Name of main module (without .lc)
    -> PipelineId -- ^ Name of pipeline to register
    -> Writer PipelineSchema a -- ^ Pipeline inputs description
    -> m ()

  -- | Removes pipeline from engine, deallocates all storages for rendering storages
  --
  -- Note: if pipeline with the name doesn't exists, do nothing.
  lambdacubeDeletePipeline :: PipelineId -> m ()

  -- | Creates new storage (corresponding to one game object)
  --
  -- Note: if pipeline not found, throws 'PipeLineNotFound'
  lambdacubeCreateStorage :: PipelineId -> m (StorageId, GLStorage)

  -- | Removes storage for pipeline, deallocates it
  --
  -- Note: if storage with the id doesn't exists, do nothing
  lambdacubeDeleteStorage :: StorageId -> m ()

  -- | Getting storage by ID 
  --
  -- Throws 'StorageNotFound' if no storage found
  lambdacubeGetStorage :: StorageId -> m GLStorage 

  -- | Adds storage to rendering queue
  lambdacubeRenderStorageLast :: StorageId -> m ()

  -- | Adds storage to rendering queue
  lambdacubeRenderStorageFirst :: StorageId -> m ()

  -- | Removes storage from rendering queue
  lambdacubeStopRendering :: StorageId -> m ()

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadLambdaCube (LambdaCubeT s m) where
  lambdacubeUpdateSize !w !h = do 
    s <- get 
    liftIO $ updateStateViewportSize w h s

  lambdacubeAddPipeline !ps !mn !pid !pwr = do 
    s <- get 
    when (isPipelineRegisteredInternal pid s) . throwM . PipeLineAlreadyRegistered $! pid
    mpd <- liftIO $ LambdaCube.compileMain ps OpenGL33 "hello"
    case mpd of 
      Left err -> throwM . PipeLineCompileFailed mn pid $! "compile error:\n" ++ err
      Right pd -> do 
        let sch = makeSchema pwr 
        r <- liftIO $ LambdaCubeGL.allocRenderer pd
        put $! registerPipelineInternal pid pd sch r s

  lambdacubeDeletePipeline !i = do 
    s <- get 
    s' <- liftIO $ unregisterPipelineInternal i s 
    put s'

  lambdacubeCreateStorage !i = do 
    s <- get 
    case getPipelineSchemeInternal i s of 
      Nothing -> throwM . PipeLineNotFound $! i 
      Just sch -> do 
        storage <- liftIO $ LambdaCubeGL.allocStorage sch 
        si <- state $ registerStorageInternal i storage
        return (si, storage)

  lambdacubeDeleteStorage !i = do 
    s <- get 
    s' <- liftIO $ unregisterStorageInternal i s 
    put s' 

  lambdacubeGetStorage !si = do 
    s <- get 
    case getStorageInternal si s of 
      Nothing -> throwM . StorageNotFound $! si 
      Just storage -> return storage 

  lambdacubeRenderStorageLast !si = do 
    s <- get 
    put $! renderStorageLastInternal si s 

  lambdacubeRenderStorageFirst !si = do 
    s <- get 
    put $! renderStorageFirstInternal si s 

  lambdacubeStopRendering !si = do 
    s <- get 
    put $! stopRenderingInternal si s

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadLambdaCube m, MonadTrans mt) => MonadLambdaCube (mt m) where 
  lambdacubeUpdateSize a b = lift $ lambdacubeUpdateSize a b
  lambdacubeAddPipeline a b c d = lift $ lambdacubeAddPipeline a b c d
  lambdacubeDeletePipeline a = lift $ lambdacubeDeletePipeline a
  lambdacubeCreateStorage a = lift $ lambdacubeCreateStorage a
  lambdacubeDeleteStorage a = lift $ lambdacubeDeleteStorage a 
  lambdacubeGetStorage a = lift $ lambdacubeGetStorage a
  lambdacubeRenderStorageLast a = lift $ lambdacubeRenderStorageLast a
  lambdacubeRenderStorageFirst a = lift $ lambdacubeRenderStorageFirst a
  lambdacubeStopRendering a = lift $ lambdacubeStopRendering a