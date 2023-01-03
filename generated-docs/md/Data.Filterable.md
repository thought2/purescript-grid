## Module Data.Filterable

#### `Filterable`

``` purescript
class (Compactable f, Functor f) <= Filterable f  where
  partitionMap :: forall a l r. (a -> Either l r) -> f a -> { left :: f l, right :: f r }
  partition :: forall a. (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
  filterMap :: forall a b. (a -> Maybe b) -> f a -> f b
  filter :: forall a. (a -> Boolean) -> f a -> f a
```

`Filterable` represents data structures which can be _partitioned_/_filtered_.

- `partitionMap` - partition a data structure based on an either predicate.
- `partition` - partition a data structure based on boolean predicate.
- `filterMap` - map over a data structure and filter based on a maybe.
- `filter` - filter a data structure based on a boolean.

Laws:
- Functor Relation: `filterMap identity ≡ compact`
- Functor Identity: `filterMap Just ≡ identity`
- Kleisli Composition: `filterMap (l <=< r) ≡ filterMap l <<< filterMap r`

- `filter ≡ filterMap <<< maybeBool`
- `filterMap p ≡ filter (isJust <<< p)`

- Functor Relation: `partitionMap identity ≡ separate`
- Functor Identity 1: `_.right <<< partitionMap Right ≡ identity`
- Functor Identity 2: `_.left <<< partitionMap Left ≡ identity`

- `f <<< partition ≡ partitionMap <<< eitherBool` where `f = \{ no, yes } -> { left: no, right: yes }`
- `f <<< partitionMap p ≡ partition (isRight <<< p)` where `f = \{ left, right } -> { no: left, yes: right}`

Default implementations are provided by the following functions:

- `partitionDefault`
- `partitionDefaultFilter`
- `partitionDefaultFilterMap`
- `partitionMapDefault`
- `filterDefault`
- `filterDefaultPartition`
- `filterDefaultPartitionMap`
- `filterMapDefault`

##### Instances
``` purescript
Filterable Array
Filterable Maybe
(Monoid m) => Filterable (Either m)
Filterable List
(Ord k) => Filterable (Map k)
```

#### `eitherBool`

``` purescript
eitherBool :: forall a. (a -> Boolean) -> a -> Either a a
```

Upgrade a boolean-style predicate to an either-style predicate mapping.

#### `partitionDefault`

``` purescript
partitionDefault :: forall f a. Filterable f => (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
```

A default implementation of `partition` using `partitionMap`.

#### `partitionDefaultFilter`

``` purescript
partitionDefaultFilter :: forall f a. Filterable f => (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
```

A default implementation of `partition` using `filter`. Note that this is
almost certainly going to be suboptimal compared to direct implementations.

#### `partitionDefaultFilterMap`

``` purescript
partitionDefaultFilterMap :: forall f a. Filterable f => (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
```

A default implementation of `partition` using `filterMap`. Note that this
is almost certainly going to be suboptimal compared to direct
implementations.

#### `partitionMapDefault`

``` purescript
partitionMapDefault :: forall f a l r. Filterable f => (a -> Either l r) -> f a -> { left :: f l, right :: f r }
```

A default implementation of `partitionMap` using `separate`. Note that this is
almost certainly going to be suboptimal compared to direct implementations.

#### `maybeBool`

``` purescript
maybeBool :: forall a. (a -> Boolean) -> a -> Maybe a
```

Upgrade a boolean-style predicate to a maybe-style predicate mapping.

#### `filterDefault`

``` purescript
filterDefault :: forall f a. Filterable f => (a -> Boolean) -> f a -> f a
```

A default implementation of `filter` using `filterMap`.

#### `filterDefaultPartition`

``` purescript
filterDefaultPartition :: forall f a. Filterable f => (a -> Boolean) -> f a -> f a
```

A default implementation of `filter` using `partition`.

#### `filterDefaultPartitionMap`

``` purescript
filterDefaultPartitionMap :: forall f a. Filterable f => (a -> Boolean) -> f a -> f a
```

A default implementation of `filter` using `partitionMap`.

#### `filterMapDefault`

``` purescript
filterMapDefault :: forall f a b. Filterable f => (a -> Maybe b) -> f a -> f b
```

A default implementation of `filterMap` using `separate`. Note that this is
almost certainly going to be suboptimal compared to direct implementations.

#### `cleared`

``` purescript
cleared :: forall f a b. Filterable f => f a -> f b
```

Filter out all values.


### Re-exported from Data.Compactable:

#### `Compactable`

``` purescript
class Compactable f  where
  compact :: forall a. f (Maybe a) -> f a
  separate :: forall l r. f (Either l r) -> { left :: f l, right :: f r }
```

`Compactable` represents data structures which can be _compacted_/_filtered_.
This is a generalization of catMaybes as a new function `compact`. `compact`
has relations with `Functor`, `Applicative`, `Monad`, `Plus`, and `Traversable`
in that we can use these classes to provide the ability to operate on a data type
by eliminating intermediate Nothings. This is useful for representing the
filtering out of values, or failure.

To be compactable alone, no laws must be satisfied other than the type signature.

If the data type is also a Functor the following should hold:

- Functor Identity: `compact <<< map Just ≡ id`

According to Kmett, (Compactable f, Functor f) is a functor from the
kleisli category of Maybe to the category of Hask.
`Kleisli Maybe -> Hask`.

If the data type is also `Applicative` the following should hold:

- `compact <<< (pure Just <*> _) ≡ id`
- `applyMaybe (pure Just) ≡ id`
- `compact ≡ applyMaybe (pure id)`

If the data type is also a `Monad` the following should hold:

- `flip bindMaybe (pure <<< Just) ≡ id`
- `compact <<< (pure <<< (Just (=<<))) ≡ id`
- `compact ≡ flip bindMaybe pure`

If the data type is also `Plus` the following should hold:

- `compact empty ≡ empty`
- `compact (const Nothing <$> xs) ≡ empty`

##### Instances
``` purescript
Compactable Maybe
(Monoid m) => Compactable (Either m)
Compactable Array
Compactable List
(Ord k) => Compactable (Map k)
```

