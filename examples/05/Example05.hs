module Main where

import Codec.Picture as Juicy
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Int
import Data.Proxy
import Matrix

import qualified Data.Vector as V

import LambdaCube.GL as LC
import LambdaCube.GL.Mesh as LC

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
  lambdacubeAddPipeline [".", "../shared"] "example05.lc" mainPipeline $ do
    defObjectArray "objects" Triangles $ do
      "position"  @: Attribute_V3F
      "normal"    @: Attribute_V3F
      "uv"        @: Attribute_V2F
    defUniforms $ do
      "modelMat"       @: M44F
      "viewMat"        @: M44F
      "projMat"        @: M44F
      "depthMVP"       @: M44F
      "diffuseTexture" @: FTexture2D
      "lightDir"       @: V3F
      "windowWidth"    @: Int
      "windowHeight"   @: Int
      "time"           @: Float


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
        & windowCfgTitle .~ pure "Gore&Ash LambdaCube example 05"
        & windowCfgConfig .~ defaultWindow {
            windowOpenGL = Just defaultOpenGL {
                glProfile = Core Normal 3 3
              }
          , windowInitialSize = SDL.V2 640 640
          }
    glMakeCurrent (win ^. windowWindow) (win ^. windowContext)
    initPipe
    tickE <- uncurry (simulateStorage win) =<< initStorage
  return ()

-- | Initialise LambaCube storages
initStorage :: forall t m . (MonadLambdaCube t m) => m (GLStorage, GPUMesh)
initStorage = do
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  gpuMesh <- liftIO $ LC.uploadMeshToGPU $ plainMesh 500 200 40
  lambdacubeRenderStorageFirst sid
  return (storage, gpuMesh)

-- | Get dynamic aspect ratio of window
windowAspect :: Reflex t => WindowWidget t -> Dynamic t Float
windowAspect win = ffor (win ^. windowSizeDyn) $ \(SDL.V2 w h) ->
  fromIntegral w / fromIntegral h

-- | Constantly update LambdaCube storage
simulateStorage :: forall t m . (MonadLambdaCube t m, TimerMonad t m)
  => WindowWidget t -> GLStorage -> GPUMesh -> m (Event t ())
simulateStorage win storage gpuMesh = do
  let dt = 1 / 60 :: Float
  tickE <- tickEvery (realToFrac dt)
  tD <- foldDyn (const (+ dt)) 0 tickE
  tickedE <- performEvent $ ffor tickE $ const $ do
    t <- sample . current $ tD
    aspect <- sample . current $ windowAspect win
    SDL.V2 w h <- sample . current $ win ^. windowSizeDyn
    liftIO $ LC.updateUniforms storage $ do
      "viewMat" @= return (cameraMatrix t)
      "projMat" @= return (projMatrix aspect)
      "lightDir" @= return lightDirection
      "windowWidth" @= return (fromIntegral w :: Int32)
      "windowHeight" @= return (fromIntegral h :: Int32)
      "time" @= return t
  --simulateCube tD storage gpuMesh
  simulateWall tD storage gpuMesh
  return tickedE

-- -- | Render cube object
-- simulateCube :: forall t m . (MonadLambdaCube t m)
--   => Dynamic t Float -> GLStorage -> GPUMesh -> m ()
-- simulateCube tD storage gpuMesh = do
--   -- upload geometry to GPU and add to pipeline input
--   obj <- liftIO $ LC.addMeshToObjectArray storage "objects" [
--       "modelMat"
--     , "diffuseTexture"
--     , "depthMVP"] gpuMesh

--   -- load image and upload texture
--   textureData <- liftIO $ do
--     Right img <- Juicy.readImage "../shared/logo.png"
--     LC.uploadTexture2DToGPU img

--   performEvent_ $ ffor (updated tD) $ \t -> liftIO $ do
--     let setter = LC.objectUniformSetter obj
--     uniformM44F "modelMat" setter $ modelMatrixCube t
--     uniformM44F "depthMVP" setter $ depthMVPCube t
--     uniformFTexture2D "diffuseTexture" setter textureData

-- | Render wall object
simulateWall :: forall t m . (MonadLambdaCube t m)
  => Dynamic t Float -> GLStorage -> GPUMesh -> m ()
simulateWall tD storage gpuMesh = do
  -- upload geometry to GPU and add to pipeline input
  obj <- liftIO $ LC.addMeshToObjectArray storage "objects" [
      "modelMat"
    , "diffuseTexture"
    , "depthMVP"] gpuMesh

  -- load image and upload texture
  textureData <- liftIO $ do
    Right img <- Juicy.readImage "../shared/dirt_tiled.jpg"
    LC.uploadTexture2DToGPU img

  performEvent_ $ ffor (updated tD) $ const $ liftIO $ do
    let setter = LC.objectUniformSetter obj
    uniformM44F "modelMat" setter modelMatrixWall
    uniformM44F "depthMVP" setter depthMVPWall
    uniformFTexture2D "diffuseTexture" setter textureData

main :: IO ()
main = runSpiderHost $ hostApp $ runModule opts (app :: AppMonad Spider ())

opts :: LambdaCubeOptions ()
opts = LambdaCubeOptions {
    lambdaOptsNext = ()
  }

-- | Generate plain mesh with normals in +Y direction
plainMesh :: Int -> Float -> Float -> LC.Mesh
plainMesh n size uvscale = Mesh {
    mAttributes = [
        ("position", A_V3F positions)
      , ("normal",   A_V3F normals)
      , ("uv",       A_V2F uvs)
      ]
  , mPrimitive = P_TrianglesI indicies
  }
  where
    s = size / fromIntegral n

    positions = V.generate (n * n) $ \i -> let
      xi = i `mod` n
      yi = i `div` n
      in LC.V3 ((fromIntegral xi - 0.5 * fromIntegral n) * s) 0 ((fromIntegral yi - 0.5 * fromIntegral n) * s)

    normals = V.replicate (n * n) $ LC.V3 0 1 0
    uvs = V.generate (n * n) $ \i -> let
      xi = i `mod` n
      yi = i `div` n
      in LC.V2 (fromIntegral xi / uvscale) (fromIntegral yi / uvscale)

    indicies = V.concat . V.toList $ V.generate ((n-1)*(n-1)) $ \i -> let
      xi = i `mod` (n-1)
      yi = i `div` (n-1)
      uni x y = fromIntegral $ x + n * y
      in [ uni (xi+1) (yi+1), uni xi (yi+1), uni xi yi
         , uni (xi+1) yi, uni (xi+1) (yi+1), uni xi yi]

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
