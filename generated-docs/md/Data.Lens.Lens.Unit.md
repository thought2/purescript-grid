## Module Data.Lens.Lens.Unit

#### `united`

``` purescript
united :: forall a. Lens' a Unit
```

There is a `Unit` in everything.
```purescript
> view united [1,2,3]
unit
> over united (\a -> a :: Unit) [1,2,3]
[1 2 3]
```


