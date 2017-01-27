module Main where

import Codec.Picture as Juicy
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Matrix

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import LambdaCube.GL as LC
import LambdaCube.GL.Mesh as LC
import LambdaCube.Linear

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.SDL as SDL
import Game.GoreAndAsh.Time

type AppStack t = LambdaCubeT t (SDLT t (LoggingT t (TimerT t (GameMonad t))))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

-- | Single application rendering pipeline
mainPipeline :: PipelineId
mainPipeline = PipelineId "mainPipeline"

-- | Load and compile LambdaCube pipeline
initPipe :: forall t m . (MonadLambdaCube t m) => m ()
initPipe = do
  lambdacubeAddPipeline [".", "../shared"] "example03.lc" mainPipeline $ do
    defObjectArray "objects" Triangles $ do
      "position"  @: Attribute_V3F
      "normal"    @: Attribute_V3F
      "uv"        @: Attribute_V2F
    defUniforms $ do
      "modelMat"       @: M44F
      "viewMat"        @: M44F
      "projMat"        @: M44F
      "diffuseTexture" @: FTexture2D
      "lightPos"       @: V3F


-- | Draw single frame with LambdaCube on SDL context
drawFrame :: forall t . (ReflexHost t, MonadIO (HostFrame t))
  => (Word -> Word -> IO ()) -- ^ Updates width and height of context for LambdaCube
  -> IO () -- ^ Action that render LambdaCube scene
  -> Window -- ^ Window we render on
  -> Renderer -- ^ Renderer of the window
  -> HostFrame t ()
drawFrame updateLambdaCubeSize renderLambdaCube win _ = do
  -- rendererDrawColor r $= SDL.V4 0 0 0 0
  -- clear r
  SDL.V2 w h <- glGetDrawableSize win
  liftIO $ do
    updateLambdaCubeSize (fromIntegral w) (fromIntegral h)
    renderLambdaCube
  glSwapWindow win

-- | Initialise window and set up render pipeline
app :: forall t m . (MonadLambdaCube t m, MonadSDL t m, TimerMonad t m) => m ()
app = do
  SDL.initializeAll
  sizeUpdater <- lambdacubeGetSizeUpdater
  renderer <- lambdacubeGetRenderer
  rec
    win <- createMainWindow tickE (drawFrame sizeUpdater renderer) $ defaultWindowCfg
        & windowCfgTitle .~ pure "Gore&Ash LambdaCube example 03"
        & windowCfgConfig .~ defaultWindow {
            windowOpenGL = Just defaultOpenGL {
                glProfile = Core Normal 3 3
              }
          }
    glMakeCurrent (win ^. windowWindow) (win ^. windowContext)
    initPipe
    tickE <- uncurry (simulateStorage $ windowAspect win) =<< initStorage
  return ()

-- | Initialise LambaCube storages
initStorage :: forall t m . (MonadLambdaCube t m) => m (GLStorage, TextureData)
initStorage = do
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  textureData <- liftIO $ do
    -- upload geometry to GPU and add to pipeline input
    _ <- LC.uploadMeshToGPU cubeMesh >>= LC.addMeshToObjectArray storage "objects" []

    -- load image and upload texture
    Right img <- Juicy.readImage "../shared/logo.png"
    LC.uploadTexture2DToGPU img

  lambdacubeRenderStorageFirst sid
  return (storage, textureData)

-- | Get dynamic aspect ratio of window
windowAspect :: Reflex t => WindowWidget t -> Dynamic t Float
windowAspect win = ffor (win ^. windowSizeDyn) $ \(SDL.V2 w h) ->
  fromIntegral w / fromIntegral h

-- | Constantly update LambdaCube storage
simulateStorage :: forall t m . (MonadLambdaCube t m, TimerMonad t m) => Dynamic t Float  -> GLStorage -> TextureData -> m (Event t ())
simulateStorage aspectD storage textureData = do
  let dt = 1 / 60 :: Float
  tickE <- tickEvery (realToFrac dt)
  tD <- foldDyn (const (+ dt)) 0 tickE
  performEvent_ $ ffor tickE $ const $ do
    t <- sample . current $ tD
    aspect <- sample . current $ aspectD
    liftIO $ LC.updateUniforms storage $ do
      "diffuseTexture" @= return textureData
      "modelMat" @= return (modelMatrix t)
      "viewMat" @= return (cameraMatrix t)
      "projMat" @= return (projMatrix aspect)
      "lightPos" @= return (LC.V3 3 3 3 :: V3F)
  return tickE

main :: IO ()
main = runSpiderHost $ hostApp $ runModule opts (app :: AppMonad Spider ())

opts :: LambdaCubeOptions ()
opts = LambdaCubeOptions {
    lambdaOptsNext = ()
  }

-- geometry data: triangles
cubeMesh :: LC.Mesh
cubeMesh = Mesh
  { mAttributes   = M.fromList
      [ ("position",  A_V3F $ V.fromList vertecies)
      , ("normal",    A_V3F $ V.fromList normals)
      , ("uv",        A_V2F $ V.fromList uvs)
      ]
  , mPrimitive    = P_Triangles
  }
  where
  vertecies = [
      v3, v2, v1, v3, v1, v0
    , v4, v7, v6, v4, v6, v5
    , v0, v1, v7, v0, v7, v4
    , v5, v6, v2, v5, v2, v3
    , v2, v6, v7, v2, v7, v1
    , v5, v3, v0, v5, v0, v4
    ]
  normals = concat [
      replicate 6 n0
    , replicate 6 n1
    , replicate 6 n2
    , replicate 6 n3
    , replicate 6 n4
    , replicate 6 n5
    ]
  uvs = concat $ replicate 6 [u1, u2, u3, u1, u3, u0]

  v0 = LC.V3 (-1) (-1) (-1)
  v1 = LC.V3 (-1)   1  (-1)
  v2 = LC.V3   1    1  (-1)
  v3 = LC.V3   1  (-1) (-1)
  v4 = LC.V3 (-1) (-1)   1
  v5 = LC.V3   1  (-1)   1
  v6 = LC.V3   1    1    1
  v7 = LC.V3 (-1)   1    1

  n0 = LC.V3   0    0  (-1)
  n1 = LC.V3   0    0    1
  n2 = LC.V3 (-1)   0    0
  n3 = LC.V3   1    0    0
  n4 = LC.V3   0    1    0
  n5 = LC.V3   0  (-1)   0

  u0 = LC.V2 0 0
  u1 = LC.V2 1 0
  u2 = LC.V2 1 1
  u3 = LC.V2 0 1


-- Boilerplate below

deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (AppMonad t)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t), MonadThrow (HostFrame t)) => MonadLambdaCube t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => TimerMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => LoggingMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t), MonadCatch (HostFrame t)) => MonadSDL t (AppMonad t)
deriving instance (ReflexHost t) => MonadSample t (AppMonad t)
deriving instance (ReflexHost t) => MonadHold t (AppMonad t)
deriving instance (ReflexHost t) => MonadSubscribeEvent t (AppMonad t)

instance ReflexHost t => MonadReflexCreateTrigger t (AppMonad t) where
  newEventWithTrigger = AppMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = AppMonad $ newFanEventWithTrigger trigger

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppMonad t) where
  getFireAsync = AppMonad getFireAsync
  getRunAppHost = do
    runner <- AppMonad getRunAppHost
    return $ \m -> runner $ runAppMonad m
  performPostBuild_ = AppMonad . performPostBuild_
  liftHostFrame = AppMonad . liftHostFrame

instance (ReflexHost t, MonadIO (HostFrame t)) => GameModule t (AppMonad t) where
  type ModuleOptions t (AppMonad t) = ModuleOptions t (AppStack t)
  runModule os m = runModule os $ runAppMonad m
  withModule t _ = withModule t (Proxy :: Proxy (AppStack t))
