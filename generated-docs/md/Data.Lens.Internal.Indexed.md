## Module Data.Lens.Internal.Indexed

This module defines the `Indexed` profunctor.

#### `Indexed`

``` purescript
newtype Indexed p i s t
  = Indexed (p (Tuple i s) t)
```

Profunctor used for `IndexedOptic`s.

##### Instances
``` purescript
Newtype (Indexed p i s t) _
(Profunctor p) => Profunctor (Indexed p i)
(Strong p) => Strong (Indexed p i)
(Choice p) => Choice (Indexed p i)
(Wander p) => Wander (Indexed p i)
```


