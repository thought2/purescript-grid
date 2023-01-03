## Module Data.Lens.Prism.Either

#### `_Left`

``` purescript
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
```

Prism for the `Left` constructor of `Either`.

#### `_Right`

``` purescript
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
```

Prism for the `Right` constructor of `Either`.


### Re-exported from Data.Profunctor.Choice:

#### `left`

``` purescript
left :: forall p a b c. Choice p => p a b -> p (Either a c) (Either b c)
```

#### `right`

``` purescript
right :: forall p a b c. Choice p => p b c -> p (Either a b) (Either a c)
```

