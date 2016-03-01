gore-and-ash-lambdacube
==================

The module provides API for something for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/TeaspotStudio/gore-and-ash-lambdacube.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `LambdaCubeT`:
``` haskell
type AppStack = ModuleStack [LambdaCubeT, ... other modules ... ] IO
```

And derive `MonadLambdaCube` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadLambdaCube)
```

Building examples
=================

The package has several examples, to build them pass `examples` flag:
```
stack install --flag gore-and-ash-lambdacube:examples
```