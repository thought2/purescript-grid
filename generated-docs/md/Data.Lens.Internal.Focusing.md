## Module Data.Lens.Internal.Focusing

This module defines the `Focusing` functor

#### `Focusing`

``` purescript
newtype Focusing m s a
  = Focusing (m (Tuple s a))
```

The functor used to zoom into `StateT`.

##### Instances
``` purescript
Newtype (Focusing m s a) _
(Functor m) => Functor (Focusing m s)
(Apply m, Semigroup s) => Apply (Focusing m s)
(Applicative m, Monoid s) => Applicative (Focusing m s)
```


