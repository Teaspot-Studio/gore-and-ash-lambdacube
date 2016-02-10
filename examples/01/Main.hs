module Main where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Proxy 
import Data.Text 

import Control.Wire 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
--import Game.GoreAndAsh.LambdaCube 
import Game.GoreAndAsh.SDL 

import Core 
import FPS 

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  fps <- makeFPSBounder 60
  firstLoop fps gs 
  where 
    firstLoop fps gs = do 
      (_, gs') <- stepGame gs $ do
        _ <- sdlCreateWindowM mainWindowName "Gore&Ash LambdaCube Example 01" defaultWindow defaultRenderer
        return ()
      gameLoop fps gs'

    gameLoop fps gs = do
      waitFPSBound fps 
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` if fromMaybe False $ gameExit <$> join mg
        then cleanupGameState gs'
        else gameLoop fps gs'

mainWindowName :: Text 
mainWindowName = "mainWindow"

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = (<|> pure Nothing) $ proc _ -> do
  closed <- isWindowClosed mainWindowName -< ()
  returnA -< Just $ Game closed

-- | Outputs True if user hits close button
isWindowClosed :: Text -> AppWire a Bool
isWindowClosed wname = hold . mapE (const True) . windowClosed wname <|> pure False