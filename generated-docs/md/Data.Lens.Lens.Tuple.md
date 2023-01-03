## Module Data.Lens.Lens.Tuple

#### `_1`

``` purescript
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
```

Lens for the first component of a `Tuple`.

#### `_2`

``` purescript
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
```

Lens for the second component of a `Tuple`.


### Re-exported from Data.Profunctor.Strong:

#### `first`

``` purescript
first :: forall p a b c. Strong p => p a b -> p (Tuple a c) (Tuple b c)
```

#### `second`

``` purescript
second :: forall p a b c. Strong p => p b c -> p (Tuple a b) (Tuple a c)
```

