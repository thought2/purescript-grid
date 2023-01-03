## Module Data.Lens.Internal.Stall

This module defines the `Stall` profunctor

#### `Stall`

``` purescript
data Stall a b s t
  = Stall (s -> b -> t) (s -> Either t a)
```

The `Stall` profunctor characterizes an `AffineTraversal`.

##### Instances
``` purescript
Functor (Stall a b s)
Profunctor (Stall a b)
Strong (Stall a b)
Choice (Stall a b)
```


