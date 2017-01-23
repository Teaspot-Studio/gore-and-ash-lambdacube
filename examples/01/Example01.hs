module Main where

import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Foreign.C
import SDL (get)

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Time

type AppStack t = LambdaCubeT t (SDLT t (LoggingT t (TimerT t (GameMonad t))))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

drawFrame :: forall t . (ReflexHost t, MonadIO (HostFrame t))
  => Window -> Renderer -> HostFrame t ()
drawFrame win r = do
  rendererDrawColor r $= V4 0 0 0 0
  clear r
  rendererDrawColor r $= V4 250 0 0 0
  ws <- getCurrentSize
  let squareRect :: Rectangle Double
      squareRect = Rectangle (P $ V2 0.1 0.1) (V2 0.8 0.8)
  fillRect r (Just $ resizeRect ws squareRect)
  glSwapWindow win
  where
    getCurrentSize :: HostFrame t (V2 CInt)
    getCurrentSize = do
      vp <- get (rendererViewport r)
      case vp of
        Nothing -> return 0
        Just (Rectangle _ s) -> return s

    resizeRect :: V2 CInt -> Rectangle Double -> Rectangle CInt
    resizeRect (V2 vw vh) (Rectangle (P (V2 x y)) (V2 w h)) = Rectangle (P (V2 x' y')) (V2 w' h')
      where
        x' = round $ x * fromIntegral vw
        y' = round $ y * fromIntegral vh
        w' = round $ w * fromIntegral vw
        h' = round $ h * fromIntegral vh

app :: forall t m . (MonadLambdaCube t m, MonadSDL t m) => m ()
app = do
  _ <- createMainWindow never drawFrame defaultWindowCfg
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
