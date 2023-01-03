## Module Control.Comonad.Store.Trans

This module defines the store comonad transformer, `StoreT`.

#### `StoreT`

``` purescript
newtype StoreT s w a
  = StoreT (Tuple (w (s -> a)) s)
```

The store comonad transformer.

This comonad transformer extends the context of a value in the base comonad so that the value
depends on a position of type `s`.

The `ComonadStore` type class describes the operations supported by this comonad.

##### Instances
``` purescript
Newtype (StoreT s w a) _
(Functor w) => Functor (StoreT s w)
(Extend w) => Extend (StoreT s w)
(Comonad w) => Comonad (StoreT s w)
ComonadTrans (StoreT s)
```

#### `runStoreT`

``` purescript
runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s
```

Unwrap a value in the `StoreT` comonad.


