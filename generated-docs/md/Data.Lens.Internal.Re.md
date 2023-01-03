## Module Data.Lens.Internal.Re

This module defines the `Re` profunctor

#### `Re`

``` purescript
newtype Re p s t a b
  = Re (p b a -> p t s)
```

##### Instances
``` purescript
Newtype (Re p s t a b) _
(Profunctor p) => Profunctor (Re p s t)
(Choice p) => Cochoice (Re p s t)
(Cochoice p) => Choice (Re p s t)
(Strong p) => Costrong (Re p s t)
(Costrong p) => Strong (Re p s t)
```


