## Module Data.Lens.Getter

This module defines functions for working with getters.

#### `(^.)`

``` purescript
infixl 8 viewOn as ^.
```

#### `viewOn`

``` purescript
viewOn :: forall s t a b. s -> AGetter s t a b -> a
```

Synonym for `view`, flipped.

#### `view`

``` purescript
view :: forall s t a b. AGetter s t a b -> s -> a
```

View the focus of a `Getter`.

#### `to`

``` purescript
to :: forall s t a b. (s -> a) -> Getter s t a b
```

Convert a function into a getter.

#### `takeBoth`

``` purescript
takeBoth :: forall s t a b c d. AGetter s t a b -> AGetter s t c d -> Getter s t (Tuple a c) (Tuple b d)
```

Combine two getters.

#### `use`

``` purescript
use :: forall s t a b m. MonadState s m => Getter s t a b -> m a
```

View the focus of a `Getter` in the state of a monad.

#### `iview`

``` purescript
iview :: forall i s t a b. IndexedFold (Tuple i a) i s t a b -> s -> Tuple i a
```

View the focus of a `Getter` and its index.

#### `iuse`

``` purescript
iuse :: forall i s t a b m. MonadState s m => IndexedFold (Tuple i a) i s t a b -> m (Tuple i a)
```

View the focus of a `Getter` and its index in the state of a monad.

#### `cloneGetter`

``` purescript
cloneGetter :: forall s t a b. AGetter s t a b -> Getter s t a b
```


### Re-exported from Data.Lens.Types:

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

#### `IndexedGetter`

``` purescript
type IndexedGetter i s t a b = IndexedFold a i s t a b
```

An indexed getter.

#### `IndexedFold`

``` purescript
type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b
```

An indexed fold.

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

#### `Getter`

``` purescript
type Getter s t a b = forall r. Fold r s t a b
```

A getter.

#### `Fold`

``` purescript
type Fold r s t a b = Optic (Forget r) s t a b
```

A fold.

#### `AGetter`

``` purescript
type AGetter s t a b = Fold a s t a b
```

