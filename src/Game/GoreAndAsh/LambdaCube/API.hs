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
import Data.Text (Text)
import GHC.Generics (Generic)

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
  exampleFunc :: m ()

instance {-# OVERLAPPABLE #-} (MonadTrans mt, MonadAppHost t (mt m), MonadThrow (mt m), MonadLambdaCube t m)
  => MonadLambdaCube t (mt m) where

  exampleFunc = lift exampleFunc
  {-# INLINE exampleFunc #-}
