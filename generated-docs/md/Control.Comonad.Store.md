## Module Control.Comonad.Store

This module defines the `Store` comonad.

#### `Store`

``` purescript
type Store s = StoreT s Identity
```

The `Store` comonad is a synonym for the `StoreT` comonad transformer, applied
to the `Identity` monad.

#### `runStore`

``` purescript
runStore :: forall s a. Store s a -> Tuple (s -> a) s
```

Unwrap a value in the `Store` comonad.

#### `store`

``` purescript
store :: forall s a. (s -> a) -> s -> Store s a
```

Create a value in context in the `Store` comonad.


### Re-exported from Control.Comonad.Store.Class:

#### `ComonadStore`

``` purescript
class (Comonad w) <= ComonadStore s w | w -> s where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a
```

The `ComonadStore` type class represents those monads which support local position information via
`pos` and `peek`.

- `pos` reads the current position.
- `peek` reads the value at the specified position in the specified context.

An implementation is provided for `StoreT`.

Laws:

- `pos (extend _ x) = pos x`
- `peek (pos x) x = extract x`

For example:

```purescript
blur :: forall w. (ComonadStore Number w) -> w Number -> w Number
blur = extend \r -> (peeks (\n -> n - 1) r + peeks (\n -> n + 1) r) / 2)
```

##### Instances
``` purescript
(Comonad w) => ComonadStore s (StoreT s w)
(ComonadStore s w) => ComonadStore s (EnvT e w)
(ComonadStore s w, Monoid m) => ComonadStore s (TracedT m w)
```

#### `seeks`

``` purescript
seeks :: forall s a w. ComonadStore s w => (s -> s) -> w a -> w a
```

Reposition the focus at the specified position, which depends on the current position.

#### `seek`

``` purescript
seek :: forall s a w. ComonadStore s w => s -> w a -> w a
```

Reposition the focus at the specified position.

#### `peeks`

``` purescript
peeks :: forall s a w. ComonadStore s w => (s -> s) -> w a -> a
```

Extract a value from a position which depends on the current position.

#### `experiment`

``` purescript
experiment :: forall f a w s. ComonadStore s w => Functor f => (s -> f s) -> w a -> f a
```

Extract a collection of values from positions which depend on the current position.

### Re-exported from Control.Comonad.Store.Trans:

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

