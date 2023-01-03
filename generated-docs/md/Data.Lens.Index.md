## Module Data.Lens.Index

#### `Index`

``` purescript
class Index m a b | m -> a, m -> b where
  ix :: a -> AffineTraversal' m b
```

`Index` is a type class whose instances are optics used when:
1. The focus element might not be present.
2. You either cannot or do not want to add new elements or delete existing ones.

`Array` is a typical example:

```purescript
preview (ix 1) [0, 1, 2] == Just 1

set (ix 1) 8888 [0, 1, 2] == [0,8888,2]
```

Note the use of `preview` rather `view`. That's because the optic is
a `Data.Lens.Traversal` tailored to the case where there's a single element
of interest.

Another common use is a `Map` that you don't want to either grow or shrink:

```purescript
(set (ix "k") "new" $ Map.singleton "k" "old") == Map.singleton "k" "new"

(set (ix "k") "new" $ Map.empty) == Map.empty
```

Note the second line: an attempt to `set` a missing focus element
leaves the original whole unchanged.

If you *do* want to add or delete elements, see `Data.Lens.At`.

##### Instances
``` purescript
(Eq i) => Index (i -> a) i a
Index (Maybe a) Unit a
Index (Identity a) Unit a
Index (Array a) Int a
Index (NonEmptyArray a) Int a
Index (List a) Int a
(Ord a) => Index (Set a) a Unit
(Ord k) => Index (Map k v) k v
Index (Object v) String v
```


