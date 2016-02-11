module Main where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad (join)
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

import qualified Graphics.UI.GLFW as GLFW 
import qualified Data.Map as Map
import qualified Data.Vector as V

--import Codec.Picture as Juicy
--import LambdaCube.Compiler as LambdaCube -- compiler
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL


main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  fps <- makeFPSBounder 60
  firstLoop fps gs 
  where 
    firstLoop fps gs = do 
      (_, gs') <- stepGame gs $ do
        win <- liftIO $ initWindow "Gore&Ash LambdaCube Example 01" 640 640
        setCurrentWindowM $ Just win 
        lambdacubeAddPipeline [".", "../shared"] "example01" mainPipeline $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V2F
            "uv"        @: Attribute_V2F
          defUniforms $ do
            "time"           @: Float
            "diffuseTexture" @: FTexture2D

        (_, storage) <- lambdacubeCreateStorage mainPipeline
        -- upload geometry to GPU and add to pipeline input
        liftIO $ do 
          _ <- LambdaCubeGL.uploadMeshToGPU triangleA >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
          _ <- LambdaCubeGL.uploadMeshToGPU triangleB >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
          return ()
          
        return ()
      gameLoop fps gs'

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

mainPipeline :: PipelineId 
mainPipeline = "mainPipeline"

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  updateWinSize -< w
  glfwFinishFrame -< w
  returnA -< Just $ Game closed

-- | Outputs True if user hits close button
isWindowClosed :: AppWire a Bool
isWindowClosed = hold . mapE (const True) . windowClosing <|> pure False

-- | Updates LambdaCube window size
updateWinSize :: AppWire GLFW.Window ()
updateWinSize = liftGameMonad1 $ \win -> do 
  (w, h) <- liftIO $ GLFW.getWindowSize win
  lambdacubeUpdateSize (fromIntegral w) (fromIntegral h)

-- | Inhibits if gets Nothing
nothingInhibit :: AppWire (Maybe a) a 
nothingInhibit = mkPure_ $ \ma -> case ma of 
  Nothing -> Left ()
  Just a -> Right a

-- | Swaps frame 
glfwFinishFrame :: AppWire GLFW.Window ()
glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- geometry data: triangles
triangleA :: LambdaCubeGL.Mesh
triangleA = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 1 (-1), V2 (-1) (-1)])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 1, V2 0 0])
        ]
    , mPrimitive    = P_Triangles
    }

triangleB :: LambdaCubeGL.Mesh
triangleB = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 (-1) (-1), V2 (-1) 1])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 0, V2 1 0])
        ]
    , mPrimitive    = P_Triangles
    }