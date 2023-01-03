## Module Control.Comonad.Env.Trans

This module defines the environment comonad transformer, `EnvT`.

#### `EnvT`

``` purescript
newtype EnvT e w a
  = EnvT (Tuple e (w a))
```

The environment comonad transformer.

This comonad transformer extends the context of a value in the base comonad with a _global environment_ of
type `e`.

The `ComonadEnv` type class describes the operations supported by this comonad.

##### Instances
``` purescript
Newtype (EnvT e w a) _
(Functor w) => Functor (EnvT e w)
(Extend w) => Extend (EnvT e w)
(Comonad w) => Comonad (EnvT e w)
ComonadTrans (EnvT e)
(Foldable f) => Foldable (EnvT e f)
(Traversable f) => Traversable (EnvT e f)
(FunctorWithIndex i w) => FunctorWithIndex i (EnvT e w)
(FoldableWithIndex i w) => FoldableWithIndex i (EnvT e w)
(TraversableWithIndex i w) => TraversableWithIndex i (EnvT e w)
```

#### `runEnvT`

``` purescript
runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
```

Unwrap a value in the `EnvT` comonad.

#### `withEnvT`

``` purescript
withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
```

Change the environment type in an `EnvT` context.

#### `mapEnvT`

``` purescript
mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
```

Change the underlying comonad and data type in an `EnvT` context.


