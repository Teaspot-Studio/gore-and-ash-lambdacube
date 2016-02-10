{-|
Module      : Game.GoreAndAsh.LambdaCube.State
Description : Internal state of core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.LambdaCube.State(
    LambdaCubeState(..)
  , PipelineId
  , StorageId(..)
  , emptyLambdaCubeState
  , freeLambdaCubeState
  -- | Internal API
  , updateStateViewportSize
  , isPipelineRegisteredInternal
  , registerPipelineInternal
  , unregisterPipelineInternal
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Text 
import GHC.Generics (Generic)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

import Data.Sequence (Seq)
import qualified Data.Sequence as S 

import LambdaCube.Compiler as LambdaCube
import LambdaCube.GL as LambdaCubeGL

-- | ID to uniquely identify LambdaCube rednering pipeline
type PipelineId = Text 

-- | ID to uniquely identify LambdaCube storage
data StorageId = StorageId { 
    storageId :: !Int 
  , storageScheme :: !PipelineId
  }
  deriving (Generic, Eq, Show)

instance NFData StorageId 
instance Hashable StorageId

-- | Check is storage binded to specified pipeline
isPipelineStorage :: PipelineId -> StorageId -> Bool 
isPipelineStorage pid sid = storageScheme sid == pid

-- | All info ablut pipeline
data PipelineInfo = PipelineInfo {
  pipeInfoRenderer :: !GLRenderer
, pipeInfoSchema :: !PipelineSchema
, pipeInfoPipeline :: !Pipeline   
} deriving Generic 

instance NFData PipelineInfo

-- | Internal state of core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data LambdaCubeState s = LambdaCubeState {
  -- | Module storage for LambdaCube pipelines
  lambdacubePipelines :: !(HashMap PipelineId PipelineInfo)
  -- | Module storage for LambdaCube storages
, lambdacubeStorages :: !(HashMap StorageId GLStorage)
  -- | Defines in which order to render each object at next frame
, lambdacubeRenderOrder :: !(Seq StorageId)
  -- | Next storage id to use
, lambdacubeNextStorageId :: !Int
  -- | Next module state in chain of modules
, lambdacubeNextState :: !s
} deriving (Generic)

instance NFData s => NFData (LambdaCubeState s)

instance NFData PipelineSchema where 
  rnf a = a `seq` () 

instance NFData GLStorage where 
  rnf a = a `seq` () 

instance NFData GLRenderer where 
  rnf a = a `seq` () 

instance NFData Pipeline where 
  rnf a = a `seq` () 

-- | Create inital state of the core module
--
-- [@s@] -  state of next module
emptyLambdaCubeState :: s -> LambdaCubeState s 
emptyLambdaCubeState s = LambdaCubeState {
    lambdacubePipelines = H.empty 
  , lambdacubeStorages = H.empty 
  , lambdacubeRenderOrder = S.empty
  , lambdacubeNextStorageId = 0
  , lambdacubeNextState = s
  }

-- | Release module state resources
freeLambdaCubeState :: LambdaCubeState s -> IO ()
freeLambdaCubeState LambdaCubeState{..} = do 
  mapM_ LambdaCubeGL.disposeStorage lambdacubeStorages
  mapM_ (LambdaCubeGL.disposeRenderer . pipeInfoRenderer) lambdacubePipelines

-- | Update viewport size of all storages
updateStateViewportSize :: Word -> Word -> LambdaCubeState s -> IO ()
updateStateViewportSize w h LambdaCubeState{..} = 
  mapM_ (\s -> LambdaCubeGL.setScreenSize s w h) $ H.elems lambdacubeStorages

-- | Returns True if given pipeline is already exists
isPipelineRegisteredInternal :: PipelineId -> LambdaCubeState s -> Bool 
isPipelineRegisteredInternal pid LambdaCubeState{..} = case H.lookup pid lambdacubePipelines of 
    Nothing -> False 
    Just _ -> True 

-- | Register new pipeline with renderer in module
registerPipelineInternal :: PipelineId -> Pipeline -> PipelineSchema -> GLRenderer -> LambdaCubeState s -> LambdaCubeState s
registerPipelineInternal i ps pl r s = s {
    lambdacubePipelines = H.insert i info . lambdacubePipelines $! s
  }
  where 
    info = PipelineInfo {
        pipeInfoRenderer = r 
      , pipeInfoSchema = pl
      , pipeInfoPipeline = ps
      }

-- | Removes pipeline from state and deletes it, also destroys all storages of the pipeline
unregisterPipelineInternal :: PipelineId -> LambdaCubeState s -> IO (LambdaCubeState s)
unregisterPipelineInternal i s =
  case H.lookup i . lambdacubePipelines $! s of 
    Nothing -> return s
    Just (PipelineInfo{..}) -> do 
      let storages = H.filterWithKey (\k _ -> isPipelineStorage i k) . lambdacubeStorages $! s
      mapM_ LambdaCubeGL.disposeStorage . H.elems $! storages
      LambdaCubeGL.disposeRenderer pipeInfoRenderer 
      return $ s {
          lambdacubePipelines = H.delete i . lambdacubePipelines $! s
        , lambdacubeStorages = H.filterWithKey (\k _ -> not $ isPipelineStorage i k) . lambdacubeStorages $! s
        }