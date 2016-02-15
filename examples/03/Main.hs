module Main where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad (join)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Proxy

import Control.Wire 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.GLFW 

import Core 
import FPS 
import Matrix

import qualified Graphics.UI.GLFW as GLFW 
import qualified Data.Map as Map
import qualified Data.Vector as V

import Codec.Picture as Juicy
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
import LambdaCube.Linear 

mainPipeline :: PipelineId 
mainPipeline = "mainPipeline"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState initStorage
  fps <- makeFPSBounder 180
  firstLoop fps gs `catch` errorExit
  where 
    firstLoop fps gs = do 
      (_, gs') <- stepGame gs $ do
        win <- liftIO $ initWindow "Gore&Ash LambdaCube Example 03" 640 640
        setCurrentWindowM $ Just win 
        lambdacubeAddPipeline [".", "../shared"] "example03" mainPipeline $ do
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

        return ()
      gameLoop fps gs'

    errorExit e = do 
      liftIO $ case e of 
        PipeLineCompileFailed _ _ msg -> putStrLn msg
        PipeLineAlreadyRegistered i -> putStrLn $ "Pipeline already registered: " ++ show i
        PipeLineNotFound i -> putStrLn $ "Pipeline is not found: " ++ show i 
        StorageNotFound i -> putStrLn $ "Storage is not found: " ++ show i 
        PipeLineIncompatible _ msg -> putStrLn $ "Pipeline incompatible: " ++ msg
      fail "terminate: fatal error"

    gameLoop fps gs = do
      waitFPSBound fps 
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` if fromMaybe False $ gameExit <$> join mg
        then cleanupGameState gs'
        else gameLoop fps gs'

initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 3
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      , GLFW.WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 

-- | Initalizes storage and then switches to rendering state
initStorage :: AppWire a (Maybe Game)
initStorage = mkGen $ \_ _ -> do 
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  textureData <- liftIO $ do 
    -- upload geometry to GPU and add to pipeline input
    _ <- LambdaCubeGL.uploadMeshToGPU cubeMesh >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
    
    -- load image and upload texture
    Right img <- Juicy.readImage "../shared/logo.png"
    LambdaCubeGL.uploadTexture2DToGPU img

  lambdacubeRenderStorageFirst sid
  return (Right Nothing, renderWire storage textureData)

-- | Infinitely render given storage
renderWire :: GLStorage -> TextureData -> AppWire a (Maybe Game)
renderWire storage textureData = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  aspect <- updateWinSize -< w
  renderStorage -< aspect
  glfwFinishFrame -< w
  returnA -< Just $ Game closed
  where
  -- | Outputs True if user hits close button
  isWindowClosed :: AppWire a Bool
  isWindowClosed = hold . mapE (const True) . windowClosing <|> pure False

  -- | Updates LambdaCube window size
  updateWinSize :: AppWire GLFW.Window Float
  updateWinSize = liftGameMonad1 $ \win -> do 
    (w, h) <- liftIO $ GLFW.getWindowSize win
    lambdacubeUpdateSize (fromIntegral w) (fromIntegral h)
    return $ fromIntegral w / fromIntegral h

  -- | Updates storage uniforms
  renderStorage :: AppWire Float ()
  renderStorage = proc aspect -> do 
    t <- timeF -< ()
    fillUniforms -< (aspect, t)
    where
    fillUniforms :: AppWire (Float, Float) ()
    fillUniforms = liftGameMonad1 $ \(aspect, t) -> liftIO $ 
      LambdaCubeGL.updateUniforms storage $ do
        "diffuseTexture" @= return textureData
        "modelMat" @= return (modelMatrix t)
        "viewMat" @= return (cameraMatrix t)
        "projMat" @= return (projMatrix aspect)
        "lightPos" @= return (V3 3 3 3 :: V3F)

  -- | Swaps frame 
  glfwFinishFrame :: AppWire GLFW.Window ()
  glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- | Inhibits if gets Nothing
nothingInhibit :: AppWire (Maybe a) a 
nothingInhibit = mkPure_ $ \ma -> case ma of 
  Nothing -> Left ()
  Just a -> Right a

-- geometry data: triangles
cubeMesh :: LambdaCubeGL.Mesh
cubeMesh = Mesh
  { mAttributes   = Map.fromList
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

  v0 = V3 (-1) (-1) (-1)
  v1 = V3 (-1)   1  (-1)
  v2 = V3   1    1  (-1)
  v3 = V3   1  (-1) (-1)
  v4 = V3 (-1) (-1)   1
  v5 = V3   1  (-1)   1
  v6 = V3   1    1    1
  v7 = V3 (-1)   1    1

  n0 = V3   0    0  (-1)
  n1 = V3   0    0    1 
  n2 = V3 (-1)   0    0
  n3 = V3   1    0    0 
  n4 = V3   0    1    0
  n5 = V3   0  (-1)   0

  u0 = V2 0 0 
  u1 = V2 1 0 
  u2 = V2 1 1 
  u3 = V2 0 1