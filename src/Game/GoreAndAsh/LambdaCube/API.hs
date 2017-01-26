{-|
Module      : Game.GoreAndAsh.LambdaCube.API
Description : API of resource module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains public API for lambda cube module.
-}
module Game.GoreAndAsh.LambdaCube.API(
    LambdaCubeException(..)
  , PipelineId(..)
  , StorageId(..)
  , isPipelineStorage
  , MonadLambdaCube(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)

import LambdaCube.Compiler as LambdaCube
import LambdaCube.GL as LambdaCubeGL

import Game.GoreAndAsh

-- | Exception type that could be thrown by the module
data LambdaCubeException =
  -- | Thrown when a pipeline compilation failed, first is pipeline main module, last is error message
    PipeLineCompileFailed Text PipelineId Text
  -- | Thrown when tries to register the same pipeline twice
  | PipeLineAlreadyRegistered PipelineId
  -- | Trhown when tries to create storage for unregistered pipeline
  | PipeLineNotFound PipelineId
  -- | Thrown when tries to get unregistered storage
  | StorageNotFound StorageId
  -- | Thrown when failed to bind pipeline to context, contains pipeline name and error message
  | PipeLineIncompatible StorageId Text
  deriving (Generic, Show)

instance Exception LambdaCubeException

-- | ID to uniquely identify LambdaCube rednering pipeline
newtype PipelineId = PipelineId { unPipelineId :: Text }
  deriving (Generic, Eq, Ord, Show)

-- | ID to uniquely identify LambdaCube storage
data StorageId = StorageId {
    storageId     :: !Int
  , storageScheme :: !PipelineId
  }
  deriving (Generic, Eq, Ord, Show)

-- | Check is storage binded to specified pipeline
isPipelineStorage :: PipelineId -> StorageId -> Bool
isPipelineStorage pid sid = storageScheme sid == pid

-- | Public API of resouce module.
--
-- You can use like a mtl type class:
--
-- @
-- foo :: (MonadLambdaCube t m, LoggingMonad t m) => m ()
-- @
class (MonadAppHost t m, MonadThrow m) => MonadLambdaCube t m | m -> t where
  -- | Update viewport size for rendering engine
  -- Should be called when window size is changed (or every frame)
  lambdacubeUpdateSize :: Word -- ^ Width of screen in pixels
    -> Word -- ^ Height of screen in pixels
    -> m ()

  -- | Get function same as 'lambdacubeUpdateSize' but operational in IO monad.
  lambdacubeGetSizeUpdater :: m (Word -> Word -> IO ())

  -- | Compile and register new pipeline.
  --
  -- Throws: 'PipeLineCompileFailed' or 'PipeLineAlreadyRegistered' when failed.
  lambdacubeAddPipeline ::
    [FilePath] -- ^ Where to find LC modules
    -> Text -- ^ Name of main module (without .lc)
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

  -- | Perform rendering of all queued storages
  lambdacubeRender :: m ()

  -- | Get function that renders current storages each call
  lambdacubeGetRenderer :: m (IO ())

instance {-# OVERLAPPABLE #-} (MonadTrans mt, MonadAppHost t (mt m), MonadThrow (mt m), MonadLambdaCube t m)
  => MonadLambdaCube t (mt m) where

  lambdacubeUpdateSize w h = lift $ lambdacubeUpdateSize w h
  {-# INLINE lambdacubeUpdateSize #-}

  lambdacubeGetSizeUpdater = lift lambdacubeGetSizeUpdater
  {-# INLINE lambdacubeGetSizeUpdater #-}

  lambdacubeAddPipeline a b c d = lift $ lambdacubeAddPipeline a b c d
  {-# INLINE lambdacubeAddPipeline #-}

  lambdacubeDeletePipeline a = lift $ lambdacubeDeletePipeline a
  {-# INLINE lambdacubeDeletePipeline #-}

  lambdacubeCreateStorage a = lift $ lambdacubeCreateStorage a
  {-# INLINE lambdacubeCreateStorage #-}

  lambdacubeDeleteStorage a = lift $ lambdacubeDeleteStorage a
  {-# INLINE lambdacubeDeleteStorage #-}

  lambdacubeGetStorage a = lift $ lambdacubeGetStorage a
  {-# INLINE lambdacubeGetStorage #-}

  lambdacubeRenderStorageLast a = lift $ lambdacubeRenderStorageLast a
  {-# INLINE lambdacubeRenderStorageLast #-}

  lambdacubeRenderStorageFirst a = lift $ lambdacubeRenderStorageFirst a
  {-# INLINE lambdacubeRenderStorageFirst #-}

  lambdacubeStopRendering a = lift $ lambdacubeStopRendering a
  {-# INLINE lambdacubeStopRendering #-}

  lambdacubeRender = lift lambdacubeRender
  {-# INLINE lambdacubeRender #-}

  lambdacubeGetRenderer = lift lambdacubeGetRenderer
  {-# INLINE lambdacubeGetRenderer #-}
