## Module Data.Lens.Internal.Tagged

This module defines the `Tagged` profunctor

#### `Tagged`

``` purescript
newtype Tagged a b
  = Tagged b
```

##### Instances
``` purescript
Newtype (Tagged a b) _
(Eq b) => Eq (Tagged a b)
Eq1 (Tagged a)
(Ord b) => Ord (Tagged a b)
Ord1 (Tagged a)
Functor (Tagged a)
Profunctor Tagged
Choice Tagged
Costrong Tagged
Closed Tagged
Foldable (Tagged a)
Traversable (Tagged a)
```


