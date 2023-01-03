## Module Data.Lens.Setter

This module defines functions for working with setters.

#### `(%~)`

``` purescript
infixr 4 over as %~
```

#### `over`

``` purescript
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
```

Apply a function to the foci of a `Setter`.

#### `iover`

``` purescript
iover :: forall i s t a b. IndexedSetter i s t a b -> (i -> a -> b) -> s -> t
```

Apply a function to the foci of a `Setter` that may vary with the index.

#### `(.~)`

``` purescript
infixr 4 set as .~
```

#### `set`

``` purescript
set :: forall s t a b. Setter s t a b -> b -> s -> t
```

Set the foci of a `Setter` to a constant value.

#### `(+~)`

``` purescript
infixr 4 addOver as +~
```

#### `addOver`

``` purescript
addOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
```

#### `(-~)`

``` purescript
infixr 4 subOver as -~
```

#### `subOver`

``` purescript
subOver :: forall s t a. Ring a => Setter s t a a -> a -> s -> t
```

#### `(*~)`

``` purescript
infixr 4 mulOver as *~
```

#### `mulOver`

``` purescript
mulOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
```

#### `(//~)`

``` purescript
infixr 4 divOver as //~
```

#### `divOver`

``` purescript
divOver :: forall s t a. EuclideanRing a => Setter s t a a -> a -> s -> t
```

#### `(||~)`

``` purescript
infixr 4 disjOver as ||~
```

#### `disjOver`

``` purescript
disjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
```

#### `(&&~)`

``` purescript
infixr 4 conjOver as &&~
```

#### `conjOver`

``` purescript
conjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
```

#### `(<>~)`

``` purescript
infixr 4 appendOver as <>~
```

#### `appendOver`

``` purescript
appendOver :: forall s t a. Semigroup a => Setter s t a a -> a -> s -> t
```

#### `(?~)`

``` purescript
infixr 4 setJust as ?~
```

#### `setJust`

``` purescript
setJust :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
```

#### `(.=)`

``` purescript
infix 4 assign as .=
```

#### `assign`

``` purescript
assign :: forall s a b m. MonadState s m => Setter s s a b -> b -> m Unit
```

Set the foci of a `Setter` in a monadic state to a constant value.

#### `(%=)`

``` purescript
infix 4 modifying as %=
```

#### `modifying`

``` purescript
modifying :: forall s a b m. MonadState s m => Setter s s a b -> (a -> b) -> m Unit
```

Modify the foci of a `Setter` in a monadic state.

#### `(+=)`

``` purescript
infix 4 addModifying as +=
```

#### `addModifying`

``` purescript
addModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
```

#### `(*=)`

``` purescript
infix 4 mulModifying as *=
```

#### `mulModifying`

``` purescript
mulModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
```

#### `(-=)`

``` purescript
infix 4 subModifying as -=
```

#### `subModifying`

``` purescript
subModifying :: forall s a m. MonadState s m => Ring a => Setter' s a -> a -> m Unit
```

#### `(//=)`

``` purescript
infix 4 divModifying as //=
```

#### `divModifying`

``` purescript
divModifying :: forall s a m. MonadState s m => EuclideanRing a => Setter' s a -> a -> m Unit
```

#### `(||=)`

``` purescript
infix 4 disjModifying as ||=
```

#### `disjModifying`

``` purescript
disjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
```

#### `(&&=)`

``` purescript
infix 4 conjModifying as &&=
```

#### `conjModifying`

``` purescript
conjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
```

#### `(<>=)`

``` purescript
infix 4 appendModifying as <>=
```

#### `appendModifying`

``` purescript
appendModifying :: forall s a m. MonadState s m => Semigroup a => Setter' s a -> a -> m Unit
```

#### `(?=)`

``` purescript
infix 4 assignJust as ?=
```

#### `assignJust`

``` purescript
assignJust :: forall s a b m. MonadState s m => Setter s s a (Maybe b) -> b -> m Unit
```


### Re-exported from Data.Lens.Types:

#### `Setter'`

``` purescript
type Setter' s a = Setter s s a a
```

#### `Setter`

``` purescript
type Setter s t a b = Optic Function s t a b
```

A setter.

#### `IndexedSetter`

``` purescript
type IndexedSetter i s t a b = IndexedOptic Function i s t a b
```

An indexed setter.

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

