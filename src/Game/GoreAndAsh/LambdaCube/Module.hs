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
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube.API

-- | Options that are passed to 'runModule' at application startup.
--
-- [@s@] The nested options of next module in stack. Options are layered the
-- similar way as parts of monad transformers.
data LambdaCubeOptions s = LambdaCubeOptions {
  lambdaOptsNext   :: s -- ^ Nested options of next game module
}

-- | Internal environment of game module
data LambdaCubeEnv t = LambdaCubeEnv {
  -- | Options that were used to create the module
  lambdaEnvOptions    :: LambdaCubeOptions ()
}

-- | Create a new environment for game module
newLambdaCubeEnv :: MonadAppHost t m => LambdaCubeOptions s -> m (LambdaCubeEnv t)
newLambdaCubeEnv opts = do
  return LambdaCubeEnv {
      lambdaEnvOptions = opts { lambdaOptsNext = () }
    }

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

instance {-# OVERLAPPING #-} MonadAppHost t m => MonadLambdaCube t (LambdaCubeT t m) where
  exampleFunc = return ()
  {-# INLINE exampleFunc #-}

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

