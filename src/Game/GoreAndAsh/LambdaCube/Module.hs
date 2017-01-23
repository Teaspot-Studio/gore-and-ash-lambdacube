{-|
Module      : Game.GoreAndAsh.LambdaCube.Module
Description : Internal implementation of public API of lambda cube game module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines implementation of lambda cube game module. You are
interested only in a 'LambdaCubeT' and 'LambdaCubeOptions' types as 'LambdaCubeT'
should be placed in your monad stack to enable 'MonadLambdaCube' API in your
application.

@
type AppStack t = LambdaCubeT t (LoggingT t (TimerT t (GameMonad t)))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)
@

And you will need some boilerplate code for instance deriving, see
`examples/Example01.hs` for full example.

-}
module Game.GoreAndAsh.LambdaCube.Module(
    LambdaCubeOptions(..)
  , LambdaCubeT(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.IORef
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Data.Sequence (Seq)
import Data.Text (Text)
import LambdaCube.Compiler as LambdaCube
import LambdaCube.GL as LambdaCubeGL

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Text as T

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube.API

-- | Helper to show values in 'Text'
showt :: Show a => a -> Text
showt = T.pack . show

-- | Options that are passed to 'runModule' at application startup.
--
-- [@s@] The nested options of next module in stack. Options are layered the
-- similar way as parts of monad transformers.
data LambdaCubeOptions s = LambdaCubeOptions {
  lambdaOptsNext   :: s -- ^ Nested options of next game module
}

-- | All info about a LambdaCube pipeline
data PipelineInfo = PipelineInfo {
  pipeInfoRenderer :: !GLRenderer
, pipeInfoSchema :: !PipelineSchema
, pipeInfoPipeline :: !Pipeline
}

-- | Internal environment of game module
data LambdaCubeEnv t = LambdaCubeEnv {
  -- | Options that were used to create the module.
  lambdaEnvOptions       :: !(LambdaCubeOptions ())
  -- | Holds known pipelines.
, lambdaEnvPipelines     :: !(IORef (Map PipelineId PipelineInfo))
  -- | Holds known storages and next free id for a new storage
, lambdaEnvStorages      :: !(IORef (Int, Map StorageId GLStorage))
  -- | Holds sequence of storages that are rendered to screen
, lambdaEnvRenderOrder   :: !(IORef (Seq StorageId))
}

-- | Create a new environment for game module
newLambdaCubeEnv :: MonadAppHost t m => LambdaCubeOptions s -> m (LambdaCubeEnv t)
newLambdaCubeEnv opts = do
  pipelines <- liftIO $ newIORef mempty
  storages <- liftIO $ newIORef (0, mempty)
  order <- liftIO $ newIORef mempty
  return LambdaCubeEnv {
      lambdaEnvOptions     = opts { lambdaOptsNext = () }
    , lambdaEnvPipelines   = pipelines
    , lambdaEnvStorages    = storages
    , lambdaEnvRenderOrder = order
    }

-- | Release module state resources
freeLambdaCubeEnv :: LambdaCubeEnv t -> IO ()
freeLambdaCubeEnv LambdaCubeEnv{..} = do
  mapM_ LambdaCubeGL.disposeStorage . snd =<< readIORef lambdaEnvStorages
  mapM_ (LambdaCubeGL.disposeRenderer . pipeInfoRenderer) =<< readIORef lambdaEnvPipelines

-- | Update viewport size of all storages
updateStateViewportSize :: Word -> Word -> LambdaCubeEnv t -> IO ()
updateStateViewportSize w h LambdaCubeEnv{..} = do
  m <- snd <$> readIORef lambdaEnvStorages
  mapM_ (\s -> LambdaCubeGL.setScreenSize s w h) m

-- | Returns True if given pipeline is already exists
isPipelineRegisteredInternal :: PipelineId -> LambdaCubeEnv t -> IO Bool
isPipelineRegisteredInternal pid LambdaCubeEnv{..} = do
  m <- readIORef lambdaEnvPipelines
  case M.lookup pid m of
    Nothing -> return False
    Just _ -> return True

-- | Register new pipeline with renderer in module
registerPipelineInternal :: PipelineId -> Pipeline -> PipelineSchema -> GLRenderer -> LambdaCubeEnv t -> IO ()
registerPipelineInternal i ps pl r LambdaCubeEnv{..} = do
  atomicModifyIORef' lambdaEnvPipelines $ (, ()) . M.insert i info
  where
    info = PipelineInfo {
        pipeInfoRenderer = r
      , pipeInfoSchema = pl
      , pipeInfoPipeline = ps
      }

-- | Removes pipeline from state and deletes it, also destroys all storages of the pipeline
unregisterPipelineInternal :: PipelineId -> LambdaCubeEnv t -> IO ()
unregisterPipelineInternal i LambdaCubeEnv{..} = do
  pipelines <- readIORef lambdaEnvPipelines
  case M.lookup i pipelines of
    Nothing -> return ()
    Just PipelineInfo{..} -> do
      storages <- snd <$> readIORef lambdaEnvStorages
      let storagesToDispose = M.filterWithKey (\k _ -> isPipelineStorage i k) storages
      mapM_ LambdaCubeGL.disposeStorage $ storagesToDispose
      LambdaCubeGL.disposeRenderer pipeInfoRenderer
      atomicModifyIORef' lambdaEnvPipelines $ (, ()) . M.delete i
      atomicModifyIORef' lambdaEnvStorages $ \(n, m) -> ((n, M.filterWithKey (\k _ -> not $ isPipelineStorage i k) m), ())

-- | Getter of pipeline scheme
getPipelineSchemeInternal :: PipelineId -> LambdaCubeEnv t -> IO (Maybe PipelineSchema)
getPipelineSchemeInternal i LambdaCubeEnv{..} = fmap pipeInfoSchema . M.lookup i <$> readIORef lambdaEnvPipelines

-- | Registering gl storage for given pipeline
registerStorageInternal :: PipelineId -> GLStorage -> LambdaCubeEnv t -> IO StorageId
registerStorageInternal pid storage LambdaCubeEnv{..} = do
  let mkId n = StorageId {
          storageId = n
        , storageScheme = pid
        }
  atomicModifyIORef lambdaEnvStorages $ \(i, m) -> let i' = mkId i
    in ((i + 1, M.insert i' storage m), i')

-- | Remove and deallocate storage
unregisterStorageInternal :: StorageId -> LambdaCubeEnv t -> IO ()
unregisterStorageInternal i LambdaCubeEnv{..} = do
  m <- snd <$> readIORef lambdaEnvStorages
  case M.lookup i m of
    Nothing -> return ()
    Just storage -> do
      LambdaCubeGL.disposeStorage storage
      atomicModifyIORef lambdaEnvStorages $ \(n, storages) -> ((n, M.delete i storages), ())

getRendererInternal :: PipelineId -> LambdaCubeEnv t -> IO (Maybe GLRenderer)
getRendererInternal i LambdaCubeEnv{..} = fmap pipeInfoRenderer . M.lookup i <$> readIORef lambdaEnvPipelines

-- | Find storage in state
getStorageInternal :: StorageId -> LambdaCubeEnv t -> IO (Maybe GLStorage)
getStorageInternal i LambdaCubeEnv{..} = M.lookup i . snd <$> readIORef lambdaEnvStorages

-- | Puts storage at end of rendering queue
renderStorageLastInternal :: StorageId -> LambdaCubeEnv t -> IO ()
renderStorageLastInternal i LambdaCubeEnv{..} = atomicModifyIORef lambdaEnvRenderOrder $ \m ->
  (, ()) $ S.filter (/= i) m S.|> i

-- | Puts storage at begining of rendering queue
renderStorageFirstInternal :: StorageId -> LambdaCubeEnv t -> IO ()
renderStorageFirstInternal i LambdaCubeEnv{..} = atomicModifyIORef lambdaEnvRenderOrder $ \m ->
  (, ()) $ i S.<| S.filter (/= i) m

-- | Removes storage from rendering queue
stopRenderingInternal :: StorageId -> LambdaCubeEnv t -> IO ()
stopRenderingInternal i LambdaCubeEnv{..} = atomicModifyIORef lambdaEnvRenderOrder $ \m ->
  (, ()) $ S.filter (/= i) m

-- | Implementation of 'MonadLambdaCube' API.
--
-- [@t@] FRP engine, you could ignore this parameter as it resolved only at main
-- function of your application.
--
-- [@m@] Underlying game modules, next layer in monad stack.
--
-- [@a@] Result of computation.
--
-- How to embed the monad into your app:
--
-- @
-- type AppStack t = LambdaCubeT t (LoggingT t (TimerT t (GameMonad t)))
--
-- newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
--   deriving (Functor, Applicative, Monad, MonadFix)
-- @
--
-- And you will need some boilerplate code for instance deriving, see
-- `examples/Example01.hs` for full example.
--
newtype LambdaCubeT t m a = LambdaCubeT { runLambdaCubeT :: ReaderT (LambdaCubeEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (LambdaCubeEnv t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance {-# OVERLAPPING #-} (MonadAppHost t m, MonadThrow m) => MonadLambdaCube t (LambdaCubeT t m) where
  lambdacubeUpdateSize !w !h = do
    s <- ask
    liftIO $ updateStateViewportSize w h s

  lambdacubeAddPipeline !ps !mn !pid !pwr = do
    s <- ask
    isRegistered <- liftIO $ isPipelineRegisteredInternal pid s
    when isRegistered . throwM . PipeLineAlreadyRegistered $! pid
    mpd <- liftIO $ LambdaCube.compileMain ps OpenGL33 (T.unpack mn)
    case mpd of
      Left err -> throwM . PipeLineCompileFailed mn pid $! "compile error:\n" <> showt err
      Right pd -> do
        let sch = makeSchema pwr
        r <- liftIO $ LambdaCubeGL.allocRenderer pd
        liftIO $ registerPipelineInternal pid pd sch r s

  lambdacubeDeletePipeline !i = do
    s <- ask
    liftIO $ unregisterPipelineInternal i s

  lambdacubeCreateStorage !i = do
    s <- ask
    mscheme <- liftIO $ getPipelineSchemeInternal i s
    case mscheme of
      Nothing -> throwM . PipeLineNotFound $! i
      Just sch -> liftIO $ do
        storage <- LambdaCubeGL.allocStorage sch
        si <- liftIO $ registerStorageInternal i storage s
        return (si, storage)

  lambdacubeDeleteStorage !i = do
    s <- ask
    liftIO $ unregisterStorageInternal i s

  lambdacubeGetStorage !si = do
    s <- ask
    ms <- liftIO $ getStorageInternal si s
    case ms of
      Nothing -> throwM . StorageNotFound $! si
      Just storage -> return storage

  lambdacubeRenderStorageLast !si = do
    s <- ask
    liftIO $ renderStorageLastInternal si s

  lambdacubeRenderStorageFirst !si = do
    s <- ask
    liftIO $ renderStorageFirstInternal si s

  lambdacubeStopRendering !si = do
    s <- ask
    liftIO $ stopRenderingInternal si s

-- Boilerplate

instance MonadTrans (LambdaCubeT t) where
  lift = LambdaCubeT . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LambdaCubeT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LambdaCubeT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (LambdaCubeT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- LambdaCubeT getRunAppHost
    return $ \m -> runner $ runLambdaCubeT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadTransControl (LambdaCubeT t) where
  type StT (LambdaCubeT t) a = StT (ReaderT (LambdaCubeEnv t)) a
  liftWith = defaultLiftWith LambdaCubeT runLambdaCubeT
  restoreT = defaultRestoreT LambdaCubeT

instance MonadBase b m => MonadBase b (LambdaCubeT t m) where
  liftBase = LambdaCubeT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (LambdaCubeT t m) where
  type StM (LambdaCubeT t m) a = ComposeSt (LambdaCubeT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LambdaCubeT t m) where
  type ModuleOptions t (LambdaCubeT t m) = LambdaCubeOptions (ModuleOptions t m)
  runModule opts (LambdaCubeT m) = do
    s <- newLambdaCubeEnv opts
    runModule (lambdaOptsNext opts) $ runReaderT m s
  withModule t _ = withModule t (Proxy :: Proxy m)

