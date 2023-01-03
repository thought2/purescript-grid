## Module Data.Lens.Internal.Grating

#### `Grating`

``` purescript
newtype Grating a b s t
  = Grating (((s -> a) -> b) -> t)
```

##### Instances
``` purescript
Newtype (Grating a b s t) _
Profunctor (Grating a b)
Closed (Grating a b)
```


