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

import Control.DeepSeq
import GHC.Generics 

import Control.Exception.Base (Exception)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.State.Strict 
import Control.Monad.Trans 
import Control.Monad.Writer (Writer)

import LambdaCube.Compiler as LambdaCube
import LambdaCube.GL as LambdaCubeGL

import Game.GoreAndAsh.LambdaCube.Module 
import Game.GoreAndAsh.LambdaCube.State 

-- | Exception type that could be thrown by the module
data LambdaCubeException =
  -- | Thrown when a pipeline compilation failed, first is pipeline main module, last is error message
    PipeLineCompileFailed String PipelineId String
  -- | Thrown when tries to register the same pipeline twice
  | PipeLineAlreadyRegistered PipelineId
  -- | Thrown when failed to bind pipeline to context, contains pipeline name and error message
  | PipeLineIncompatible PipelineId String
  deriving (Generic, Show)

instance Exception LambdaCubeException

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
    -> String -- ^ Name of main module (without of .lc)
    -> PipelineId -- ^ Name of pipeline to register
    -> Writer PipelineSchema a -- ^ Pipeline inputs description
    -> m ()

  -- | Removes pipeline from engine, deallocates all storages for rendering storages
  --
  -- Note: if pipeline with the name doesn't exists, do nothing.
  lambdacubeDeletePipeline :: PipelineId -> m ()

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadLambdaCube (LambdaCubeT s m) where
  lambdacubeUpdateSize w h = do 
    s <- get 
    liftIO $ updateStateViewportSize w h s

  lambdacubeAddPipeline ps mn pid pwr = do 
    s <- get 
    when (isPipelineRegisteredInternal pid s) . throwM . PipeLineAlreadyRegistered $! pid
    mpd <- liftIO $ LambdaCube.compileMain ["."] OpenGL33 "hello"
    case mpd of 
      Left err -> throwM . PipeLineCompileFailed mn pid $! "compile error:\n" ++ err
      Right pd -> do 
        let sch = makeSchema pwr 
        r <- liftIO $ LambdaCubeGL.allocRenderer pd
        put $! registerPipelineInternal pid pd sch r s

  lambdacubeDeletePipeline i = do 
    s <- get 
    s' <- liftIO $ unregisterPipelineInternal i s 
    put s'

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadLambdaCube m, MonadTrans mt) => MonadLambdaCube (mt m) where 
  lambdacubeUpdateSize a b = lift $ lambdacubeUpdateSize a b
  lambdacubeAddPipeline a b c d = lift $ lambdacubeAddPipeline a b c d
  lambdacubeDeletePipeline a = lift $ lambdacubeDeletePipeline a