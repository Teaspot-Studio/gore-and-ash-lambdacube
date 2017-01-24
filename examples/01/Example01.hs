module Main where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Foreign.C
import SDL (get)

import Codec.Picture as Juicy
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
  lambdacubeAddPipeline [".", "../shared"] "example01.lc" mainPipeline $ do
    defObjectArray "objects" Triangles $ do
      "position"  @: Attribute_V2F
      "uv"        @: Attribute_V2F
    defUniforms $ do
      "time"           @: Float
      "diffuseTexture" @: FTexture2D

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
app :: forall t m . (MonadLambdaCube t m, MonadSDL t m) => m ()
app = do
  SDL.initializeAll
  sizeUpdater <- lambdacubeGetSizeUpdater
  renderer <- lambdacubeGetRenderer
  win <- createMainWindow never (drawFrame sizeUpdater renderer) $ defaultWindowCfg
      & windowCfgConfig .~ defaultWindow {
          windowOpenGL = Just defaultOpenGL {
              glProfile = Core Normal 3 3
            }
        }
  glMakeCurrent (win ^. windowWindow) (win ^. windowContext)
  initPipe
  return ()

main :: IO ()
main = runSpiderHost $ hostApp $ runModule opts (app :: AppMonad Spider ())

opts :: LambdaCubeOptions ()
opts = LambdaCubeOptions {
    lambdaOptsNext = ()
  }

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
