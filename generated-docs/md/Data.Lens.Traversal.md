## Module Data.Lens.Traversal

`Traversal` is an optic that focuses on zero or more values. An
`Array` would be a typical example:

```purescript
over    traversed negate [1, 2, 3] == [-1, -2, -3]
preview traversed [1, 2, 3] == Just 1
firstOf traversed [1, 2, 3] == Just 1  -- same as `preview`
lastOf  traversed [1, 2, 3] == Just 3
```

`view` might surprise you. It assumes that the wrapped values
are a monoid, and `append`s them together:

```purescript
view traversed ["D", "a", "w", "n"] == "Dawn"
```

Many of the functions you'll use are documented in `Data.Lens.Fold`.

#### `traversed`

``` purescript
traversed :: forall t a b. Traversable t => Traversal (t a) (t b) a b
```

A `Traversal` for the elements of a `Traversable` functor.

```purescript
over traversed negate [1, 2, 3] == [-1,-2,-3]
over traversed negate (Just 3) == Just -3
```

#### `element`

``` purescript
element :: forall p s t a. Wander p => Int -> Traversal s t a a -> Optic p s t a a
```

Combine an index and a traversal to narrow the focus to a single
element. Compare to `Data.Lens.Index`.

```purescript
set     (element 2 traversed) 8888 [0, 0, 3] == [0, 0, 8888]
preview (element 2 traversed)      [0, 0, 3] == Just 3
```
The resulting traversal is called an *affine traversal*, which
means that the traversal focuses on one or zero (if the index is out of range)
results.

#### `traverseOf`

``` purescript
traverseOf :: forall f s t a b. Optic (Star f) s t a b -> (a -> f b) -> s -> f t
```

Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.

#### `sequenceOf`

``` purescript
sequenceOf :: forall f s t a. Optic (Star f) s t (f a) a -> s -> f t
```

Sequence the foci of an optic, pulling out an "effect".
If you do not need the result, see `sequenceOf_` for `Fold`s.

`sequenceOf traversed` has the same result as `Data.Traversable.sequence`:

```purescript
sequenceOf traversed (Just [1, 2]) == [Just 1, Just 2]
sequence             (Just [1, 2]) == [Just 1, Just 2]
```

An example with effects:
```purescript
> array = [random, random]
> :t array
Array (Eff ... Number)

> effect = sequenceOf traversed array
> :t effect
Eff ... (Array Number)

> effect >>= logShow
[0.15556037108154985,0.28500369615270515]
unit
```

#### `failover`

``` purescript
failover :: forall f s t a b. Alternative f => Optic (Star (Tuple (Disj Boolean))) s t a b -> (a -> b) -> s -> f t
```

Tries to map over a `Traversal`; returns `empty` if the traversal did
not have any new focus.

#### `elementsOf`

``` purescript
elementsOf :: forall p i s t a. Wander p => IndexedTraversal i s t a a -> (i -> Boolean) -> IndexedOptic p i s t a a
```

Traverse elements of an `IndexedTraversal` whose index satisfy a predicate.

#### `itraverseOf`

``` purescript
itraverseOf :: forall f i s t a b. IndexedOptic (Star f) i s t a b -> (i -> a -> f b) -> s -> f t
```

Turn a pure profunctor `IndexedTraversal` into a `lens`-like `IndexedTraversal`.

#### `iforOf`

``` purescript
iforOf :: forall f i s t a b. IndexedOptic (Star f) i s t a b -> s -> (i -> a -> f b) -> f t
```

Flipped version of `itraverseOf`.

#### `cloneTraversal`

``` purescript
cloneTraversal :: forall s t a b. ATraversal s t a b -> Traversal s t a b
```

#### `both`

``` purescript
both :: forall r a b. Bitraversable r => Traversal (r a a) (r b b) a b
```


### Re-exported from Data.Lens.Types:

#### `Traversal'`

``` purescript
type Traversal' s a = Traversal s s a a
```

#### `Traversal`

``` purescript
type Traversal s t a b = forall p. Wander p => Optic p s t a b
```

A traversal.

