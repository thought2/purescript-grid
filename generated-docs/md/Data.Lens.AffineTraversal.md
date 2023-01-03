## Module Data.Lens.AffineTraversal

This module defines functions for working with affine traversals.
An `AffineTraversal` is a `Traversal` that applies to at most one element.

These arise most frequently as the composition of a `Lens` with a `Prism`.

#### `affineTraversal`

``` purescript
affineTraversal :: forall s t a b. (s -> b -> t) -> (s -> Either t a) -> AffineTraversal s t a b
```

#### `affineTraversal'`

``` purescript
affineTraversal' :: forall s t a b. (s -> Tuple (b -> t) (Either t a)) -> AffineTraversal s t a b
```

#### `withAffineTraversal`

``` purescript
withAffineTraversal :: forall s t a b r. AnAffineTraversal s t a b -> ((s -> b -> t) -> (s -> Either t a) -> r) -> r
```

#### `cloneAffineTraversal`

``` purescript
cloneAffineTraversal :: forall s t a b. AnAffineTraversal s t a b -> AffineTraversal s t a b
```


### Re-exported from Data.Lens.Types:

#### `AnAffineTraversal'`

``` purescript
type AnAffineTraversal' s a = AnAffineTraversal s s a a
```

#### `AnAffineTraversal`

``` purescript
type AnAffineTraversal s t a b = Optic (Stall a b) s t a b
```

An affine traversal defined in terms of `Stall`, which can be used
to avoid issues with impredicativity.

#### `AffineTraversal'`

``` purescript
type AffineTraversal' s a = AffineTraversal s s a a
```

#### `AffineTraversal`

``` purescript
type AffineTraversal s t a b = forall p. Strong p => Choice p => Optic p s t a b
```

An affine traversal (has at most one focus, but is not a prism).

