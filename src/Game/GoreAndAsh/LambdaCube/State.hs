{-|
Module      : Game.GoreAndAsh.LambdaCube.State
Description : Internal state of core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.LambdaCube.State(
    LambdaCubeState(..)
  , emptyLambdaCubeState
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)

-- | Internal state of core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data LambdaCubeState s = LambdaCubeState {
  -- | Next module state in chain of modules
  lambdacubeNextState :: !s
} deriving (Generic)

instance NFData s => NFData (LambdaCubeState s)

-- | Create inital state of the core module
--
-- [@s@] -  state of next module
emptyLambdaCubeState :: s -> LambdaCubeState s 
emptyLambdaCubeState s = LambdaCubeState {
    lambdacubeNextState = s
  }
