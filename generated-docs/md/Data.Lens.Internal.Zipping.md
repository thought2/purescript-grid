## Module Data.Lens.Internal.Zipping

#### `Zipping`

``` purescript
newtype Zipping a b
  = Zipping (a -> a -> b)
```

##### Instances
``` purescript
Newtype (Zipping a b) _
Profunctor Zipping
Closed Zipping
```


