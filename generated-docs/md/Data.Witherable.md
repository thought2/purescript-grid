## Module Data.Witherable

#### `Witherable`

``` purescript
class (Filterable t, Traversable t) <= Witherable t  where
  wilt :: forall m a l r. Applicative m => (a -> m (Either l r)) -> t a -> m { left :: t l, right :: t r }
  wither :: forall m a b. Applicative m => (a -> m (Maybe b)) -> t a -> m (t b)
```

`Witherable` represents data structures which can be _partitioned_ with
effects in some `Applicative` functor.

- `wilt` - partition a structure with effects
- `wither` - filter a structure  with effects

Laws:

- Naturality: `t <<< wither f ≡ wither (t <<< f)`
- Identity: `wither (pure <<< Just) ≡ pure`
- Composition: `Compose <<< map (wither f) <<< wither g ≡ wither (Compose <<< map (wither f) <<< g)`
- Multipass partition: `wilt p ≡ map separate <<< traverse p`
- Multipass filter: `wither p ≡ map compact <<< traverse p`

Superclass equivalences:

- `partitionMap p = runIdentity <<< wilt (Identity <<< p)`
- `filterMap p = runIdentity <<< wither (Identity <<< p)`
- `traverse f ≡ wither (map Just <<< f)`

Default implementations are provided by the following functions:

- `wiltDefault`
- `witherDefault`
- `partitionMapByWilt`
- `filterMapByWither`
- `traverseByWither`

##### Instances
``` purescript
Witherable Array
Witherable List
(Ord k) => Witherable (Map k)
Witherable Maybe
(Monoid m) => Witherable (Either m)
```

#### `partitionMapByWilt`

``` purescript
partitionMapByWilt :: forall t a l r. Witherable t => (a -> Either l r) -> t a -> { left :: t l, right :: t r }
```

A default implementation of `partitionMap` given a `Witherable`.

#### `filterMapByWither`

``` purescript
filterMapByWither :: forall t a b. Witherable t => (a -> Maybe b) -> t a -> t b
```

A default implementation of `filterMap` given a `Witherable`.

#### `traverseByWither`

``` purescript
traverseByWither :: forall t m a b. Witherable t => Applicative m => (a -> m b) -> t a -> m (t b)
```

A default implementation of `traverse` given a `Witherable`.

#### `wilted`

``` purescript
wilted :: forall t m l r. Witherable t => Applicative m => t (m (Either l r)) -> m { left :: t l, right :: t r }
```

Partition between `Left` and `Right` values - with effects in `m`.

#### `withered`

``` purescript
withered :: forall t m x. Witherable t => Applicative m => t (m (Maybe x)) -> m (t x)
```

Filter out all the `Nothing` values - with effects in `m`.

#### `witherDefault`

``` purescript
witherDefault :: forall t m a b. Witherable t => Applicative m => (a -> m (Maybe b)) -> t a -> m (t b)
```

A default implementation of `wither` using `compact`.

#### `wiltDefault`

``` purescript
wiltDefault :: forall t m a l r. Witherable t => Applicative m => (a -> m (Either l r)) -> t a -> m { left :: t l, right :: t r }
```

A default implementation of `wilt` using `separate`


### Re-exported from Data.Filterable:

#### `Filterable`

``` purescript
class (Compactable f, Functor f) <= Filterable f 
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

