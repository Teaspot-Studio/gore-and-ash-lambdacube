{-|
Module      : Game.GoreAndAsh.LambdaCube
Description : Top module of game module for Gore&Ash engine for lambda cube handling.
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.LambdaCube(
    MonadLambdaCube(..)
  , LambdaCubeT(..)
  , LambdaCubeOptions(..)
  ) where

import Game.GoreAndAsh.LambdaCube.Module
import Game.GoreAndAsh.LambdaCube.API