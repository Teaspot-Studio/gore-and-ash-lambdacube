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
  , updateStateViewportSize
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Text 
import GHC.Generics (Generic)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

import Data.Sequence (Seq)
import qualified Data.Sequence as S 

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

-- | Internal state of core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data LambdaCubeState s = LambdaCubeState {
  -- | Module storage for LambdaCube pipelines
  lambdacubePipelines :: !(HashMap PipelineId (PipelineSchema, GLRenderer))
  -- | Module storage for LambdaCube storages
, lambdacubeStorages :: !(HashMap StorageId GLStorage)
  -- | Defines in which order to render each object at next frame
, lambdacubeRenderOrder :: !(Seq StorageId)
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

-- | Create inital state of the core module
--
-- [@s@] -  state of next module
emptyLambdaCubeState :: s -> LambdaCubeState s 
emptyLambdaCubeState s = LambdaCubeState {
    lambdacubePipelines = H.empty 
  , lambdacubeStorages = H.empty 
  , lambdacubeRenderOrder = S.empty
  , lambdacubeNextState = s
  }

-- | Release module state resources
freeLambdaCubeState :: LambdaCubeState s -> IO ()
freeLambdaCubeState LambdaCubeState{..} = do 
  mapM_ LambdaCubeGL.disposeStorage lambdacubeStorages
  mapM_ (LambdaCubeGL.disposeRenderer . snd) lambdacubePipelines

-- | Update viewport size of all storages
updateStateViewportSize :: Int -> Int -> LambdaCubeState s -> IO ()
updateStateViewportSize w h LambdaCubeState{..} = 
  mapM_ (\s -> LambdaCubeGL.setScreenSize s (fromIntegral w) (fromIntegral h)) $ H.elems lambdacubeStorages