## Module Data.Lens.Iso

This module defines functions for working with isomorphisms.

#### `iso`

``` purescript
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
```

Create an `Iso` from a pair of morphisms.

#### `withIso`

``` purescript
withIso :: forall s t a b r. AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
```

Extracts the pair of morphisms from an isomorphism.

#### `cloneIso`

``` purescript
cloneIso :: forall s t a b. AnIso s t a b -> Iso s t a b
```

Extracts an `Iso` from `AnIso`.

#### `re`

``` purescript
re :: forall p s t a b. Optic (Re p a b) s t a b -> Optic p b a t s
```

Reverses an optic.

#### `au`

``` purescript
au :: forall s t a b e. AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
```

#### `auf`

``` purescript
auf :: forall s t a b e r p. Profunctor p => AnIso s t a b -> (p r a -> e -> b) -> p r s -> e -> t
```

#### `under`

``` purescript
under :: forall s t a b. AnIso s t a b -> (t -> s) -> b -> a
```

#### `non`

``` purescript
non :: forall a. Eq a => a -> Iso' (Maybe a) a
```

If `a1` is obtained from `a` by removing a single value, then
`Maybe a1` is isomorphic to `a`.

#### `curried`

``` purescript
curried :: forall a b c d e f. Iso (Tuple a b -> c) (Tuple d e -> f) (a -> b -> c) (d -> e -> f)
```

#### `uncurried`

``` purescript
uncurried :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (Tuple a b -> c) (Tuple d e -> f)
```

#### `flipped`

``` purescript
flipped :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
```

#### `mapping`

``` purescript
mapping :: forall s t a b f g. Functor f => Functor g => AnIso s t a b -> Iso (f s) (g t) (f a) (g b)
```

#### `dimapping`

``` purescript
dimapping :: forall s ss t tt a aa b bb p q. Profunctor p => Profunctor q => AnIso s t a b -> AnIso ss tt aa bb -> Iso (p a ss) (q b tt) (p s aa) (q t bb)
```

#### `coerced`

``` purescript
coerced :: forall s t a b. Coercible s a => Coercible t b => Iso s t a b
```

An `Iso` between two types that are inferred to have the
same representation by the compiler. One scenario that this is
particularly useful is the case of nested newtypes:

```purescript
 newtype UserId = UserId Int
 newtype DeletedUserId = DeletedUserId UserId

 -- `simple` is used to aid the type inference
 deletedUser :: DeletedUserId
 deletedUser = review (simple coerced) 42
```


### Re-exported from Data.Lens.Types:

#### `Re`

``` purescript
newtype Re p s t a b
  = Re (p b a -> p t s)
```

##### Instances
``` purescript
Newtype (Re p s t a b) _
(Profunctor p) => Profunctor (Re p s t)
(Choice p) => Cochoice (Re p s t)
(Cochoice p) => Choice (Re p s t)
(Strong p) => Costrong (Re p s t)
(Costrong p) => Strong (Re p s t)
```

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

#### `Iso'`

``` purescript
type Iso' s a = Iso s s a a
```

#### `Iso`

``` purescript
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

A generalized isomorphism.

#### `Exchange`

``` purescript
data Exchange a b s t
  = Exchange (s -> a) (b -> t)
```

The `Exchange` profunctor characterizes an `Iso`.

##### Instances
``` purescript
Functor (Exchange a b s)
Profunctor (Exchange a b)
```

#### `AnIso'`

``` purescript
type AnIso' s a = AnIso s s a a
```

#### `AnIso`

``` purescript
type AnIso s t a b = Optic (Exchange a b) s t a b
```

An isomorphism defined in terms of `Exchange`, which can be used
to avoid issues with impredicativity.

