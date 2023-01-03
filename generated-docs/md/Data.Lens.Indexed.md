## Module Data.Lens.Indexed

#### `unIndex`

``` purescript
unIndex :: forall p i s t a b. Profunctor p => IndexedOptic p i s t a b -> Optic p s t a b
```

Converts an `IndexedOptic` to an `Optic` by forgetting indices.

#### `asIndex`

``` purescript
asIndex :: forall p i s t a b. Profunctor p => IndexedOptic p i s t a b -> Optic p s t i b
```

#### `reindexed`

``` purescript
reindexed :: forall p i j r a b. Profunctor p => (i -> j) -> (Indexed p i a b -> r) -> Indexed p j a b -> r
```

Remap the index.

#### `iwander`

``` purescript
iwander :: forall i s t a b. (forall f. Applicative f => (i -> a -> f b) -> s -> f t) -> IndexedTraversal i s t a b
```

Converts a `lens`-like indexed traversal to an `IndexedTraversal`.

#### `itraversed`

``` purescript
itraversed :: forall i t a b. TraversableWithIndex i t => IndexedTraversal i (t a) (t b) a b
```

Traverses over a `TraversableWithIndex` container.

#### `positions`

``` purescript
positions :: forall s t a b. Traversal s t a b -> IndexedTraversal Int s t a b
```

Converts a `Traversal` to an `IndexedTraversal` by using the integer
positions as indices.


