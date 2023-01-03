## Module Data.Lens.At

#### `At`

``` purescript
class (Index m a b) <= At m a b | m -> a, m -> b where
  at :: a -> Lens' m (Maybe b)
```

`At` is a type class whose instances let you add
new elements or delete old ones from "container-like" types:

```purescript
whole = Map.singleton "key" "value"
optic = at "key"

view optic whole == Just "value"

set optic (Just "NEW") whole == Map.singleton "key" "NEW"

set optic Nothing whole == Map.empty
```

If you don't want to add or delete, but only to view or change
an existing element, see `Data.Lens.Index`.

##### Instances
``` purescript
At (Identity a) Unit a
At (Maybe a) Unit a
(Ord v) => At (Set v) v Unit
(Ord k) => At (Map k v) k v
At (Object v) String v
```

#### `sans`

``` purescript
sans :: forall m a b. At m a b => a -> m -> m
```


