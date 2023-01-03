## Module Data.Lens.Types

This module defines types for working with lenses.

All optics have their normal name (e.g. `Lens`)
and one whose name is prefixed with either "A"
or "An" (e.g. `ALens`). Prefixed versions avoid the
issue of "impredicativity". To understand that concept
more and why prefixed names are sometimes necessary,
see the `./docs` folder.

#### `AGetter`

``` purescript
type AGetter s t a b = Fold a s t a b
```

#### `AGetter'`

``` purescript
type AGetter' s a = AGetter s s a a
```

#### `AGrate`

``` purescript
type AGrate s t a b = Optic (Grating a b) s t a b
```

A grate defined in terms of `Grating`, which can be used
to avoid issues with impredicativity.

#### `AGrate'`

``` purescript
type AGrate' s a = AGrate s s a a
```

#### `ALens`

``` purescript
type ALens s t a b = Optic (Shop a b) s t a b
```

A lens defined in terms of `Shop`, which can be used
to avoid issues with impredicativity.

#### `ALens'`

``` purescript
type ALens' s a = ALens s s a a
```

#### `APrism`

``` purescript
type APrism s t a b = Optic (Market a b) s t a b
```

A prism defined in terms of `Market` to be safe from impredicativity
issues in the type checker. See the `docs/` folder for a more detailed
explanation.

#### `APrism'`

``` purescript
type APrism' s a = APrism s s a a
```

#### `ATraversal`

``` purescript
type ATraversal s t a b = Optic (Bazaar Function a b) s t a b
```

A traversal defined in terms of `Bazaar`, which can be used
to avoid issues with impredicativity.

#### `ATraversal'`

``` purescript
type ATraversal' s a = ATraversal s s a a
```

#### `AffineTraversal`

``` purescript
type AffineTraversal s t a b = forall p. Strong p => Choice p => Optic p s t a b
```

An affine traversal (has at most one focus, but is not a prism).

#### `AffineTraversal'`

``` purescript
type AffineTraversal' s a = AffineTraversal s s a a
```

#### `AnAffineTraversal`

``` purescript
type AnAffineTraversal s t a b = Optic (Stall a b) s t a b
```

An affine traversal defined in terms of `Stall`, which can be used
to avoid issues with impredicativity.

#### `AnAffineTraversal'`

``` purescript
type AnAffineTraversal' s a = AnAffineTraversal s s a a
```

#### `AnIndexedLens`

``` purescript
type AnIndexedLens i s t a b = IndexedOptic (Shop (Tuple i a) b) i s t a b
```

An indexed lens defined in terms of `Shop`, which can be used
to avoid issues with impredicativity.

#### `AnIndexedLens'`

``` purescript
type AnIndexedLens' i s a = AnIndexedLens i s s a a
```

#### `AnIso`

``` purescript
type AnIso s t a b = Optic (Exchange a b) s t a b
```

An isomorphism defined in terms of `Exchange`, which can be used
to avoid issues with impredicativity.

#### `AnIso'`

``` purescript
type AnIso' s a = AnIso s s a a
```

#### `Fold`

``` purescript
type Fold r s t a b = Optic (Forget r) s t a b
```

A fold.

#### `Fold'`

``` purescript
type Fold' r s a = Fold r s s a a
```

#### `Getter`

``` purescript
type Getter s t a b = forall r. Fold r s t a b
```

A getter.

#### `Getter'`

``` purescript
type Getter' s a = Getter s s a a
```

#### `Grate`

``` purescript
type Grate s t a b = forall p. Closed p => Optic p s t a b
```

A grate (http://r6research.livejournal.com/28050.html)

#### `Grate'`

``` purescript
type Grate' s a = Grate s s a a
```

#### `IndexedFold`

``` purescript
type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b
```

An indexed fold.

#### `IndexedFold'`

``` purescript
type IndexedFold' r i s a = IndexedFold r i s s a a
```

#### `IndexedGetter`

``` purescript
type IndexedGetter i s t a b = IndexedFold a i s t a b
```

An indexed getter.

#### `IndexedGetter'`

``` purescript
type IndexedGetter' i s a = IndexedGetter i s s a a
```

#### `IndexedLens`

``` purescript
type IndexedLens i s t a b = forall p. Strong p => IndexedOptic p i s t a b
```

An indexed lens.

#### `IndexedLens'`

``` purescript
type IndexedLens' i s a = IndexedLens i s s a a
```

#### `IndexedOptic`

``` purescript
type IndexedOptic p i s t a b = Indexed p i a b -> p s t
```

An indexed optic.

#### `IndexedOptic'`

``` purescript
type IndexedOptic' p i s a = IndexedOptic p i s s a a
```

#### `IndexedSetter`

``` purescript
type IndexedSetter i s t a b = IndexedOptic Function i s t a b
```

An indexed setter.

#### `IndexedSetter'`

``` purescript
type IndexedSetter' i s a = IndexedSetter i s s a a
```

#### `IndexedTraversal`

``` purescript
type IndexedTraversal i s t a b = forall p. Wander p => IndexedOptic p i s t a b
```

An indexed traversal.

#### `IndexedTraversal'`

``` purescript
type IndexedTraversal' i s a = IndexedTraversal i s s a a
```

#### `Iso`

``` purescript
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

A generalized isomorphism.

#### `Iso'`

``` purescript
type Iso' s a = Iso s s a a
```

#### `Lens`

``` purescript
type Lens s t a b = forall p. Strong p => Optic p s t a b
```

Given a type whose "focus element" always exists,
a lens provides a convenient way to view, set, and transform
that element.

For example, `_2` is a tuple-specific `Lens` available from `Data.Lens`, so:
```purescript
over _2 String.length $ Tuple "ignore" "four" == Tuple "ignore" 4
```
Note the result has a different type than the original tuple.
That is, the four `Lens` type variables have been narrowed to:

* `s` is `Tuple String String`
* `t` is `Tuple String Int`
* `a` is `String`
* `b` is `Int`

See `Data.Lens.Getter` and `Data.Lens.Setter` for functions and operators
frequently used with lenses.

#### `Lens'`

``` purescript
type Lens' s a = Lens s s a a
```

`Lens'` is a specialization of `Lens`. An optic of type `Lens'`
can change only the value of its focus,
not its type. As an example, consider the `Lens` `_2`, which has this type:

```purescript
_2 :: forall s t a b. Lens (Tuple s a) (Tuple t b) a b
```

`_2` can produce a `Tuple Int String` from a `Tuple Int Int`:

```purescript
set _2 "NEW" (Tuple 1 2) == (Tuple 1 "NEW")
```

If we specialize `_2`'s type with `Lens'`, the following will not
type check:

```purescript
set (_2 :: Lens' (Tuple Int Int) Int) "NEW" (Tuple 1 2)
           ^^^^^^^^^^^^^^^^^^^^^^^^^
```

See `Data.Lens.Getter` and `Data.Lens.Setter` for functions and operators
frequently used with lenses.

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

#### `Optic'`

``` purescript
type Optic' p s a = Optic p s s a a
```

#### `Prism`

``` purescript
type Prism s t a b = forall p. Choice p => Optic p s t a b
```

A prism.

#### `Prism'`

``` purescript
type Prism' s a = Prism s s a a
```

#### `Review`

``` purescript
type Review s t a b = Optic Tagged s t a b
```

A review.

#### `Review'`

``` purescript
type Review' s a = Review s s a a
```

#### `Setter`

``` purescript
type Setter s t a b = Optic Function s t a b
```

A setter.

#### `Setter'`

``` purescript
type Setter' s a = Setter s s a a
```

#### `Traversal`

``` purescript
type Traversal s t a b = forall p. Wander p => Optic p s t a b
```

A traversal.

#### `Traversal'`

``` purescript
type Traversal' s a = Traversal s s a a
```


### Re-exported from Data.Lens.Internal.Exchange:

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

### Re-exported from Data.Lens.Internal.Forget:

#### `Forget`

``` purescript
newtype Forget r a b
  = Forget (a -> r)
```

Profunctor that forgets the `b` value and returns (and accumulates) a
value of type `r`.

`Forget r` is isomorphic to `Star (Const r)`, but can be given a `Cochoice`
instance.

##### Instances
``` purescript
Newtype (Forget r a b) _
(Semigroup r) => Semigroup (Forget r a b)
(Monoid r) => Monoid (Forget r a b)
Profunctor (Forget r)
(Monoid r) => Choice (Forget r)
Strong (Forget r)
Cochoice (Forget r)
(Monoid r) => Wander (Forget r)
```

### Re-exported from Data.Lens.Internal.Grating:

#### `Grating`

``` purescript
newtype Grating a b s t
```

##### Instances
``` purescript
Newtype (Grating a b s t) _
Profunctor (Grating a b)
Closed (Grating a b)
```

### Re-exported from Data.Lens.Internal.Indexed:

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

### Re-exported from Data.Lens.Internal.Market:

#### `Market`

``` purescript
data Market a b s t
  = Market (b -> t) (s -> Either t a)
```

The `Market` profunctor characterizes a `Prism`.

##### Instances
``` purescript
Functor (Market a b s)
Profunctor (Market a b)
Choice (Market a b)
```

### Re-exported from Data.Lens.Internal.Re:

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

### Re-exported from Data.Lens.Internal.Shop:

#### `Shop`

``` purescript
data Shop a b s t
  = Shop (s -> a) (s -> b -> t)
```

The `Shop` profunctor characterizes a `Lens`.

##### Instances
``` purescript
Profunctor (Shop a b)
Strong (Shop a b)
```

### Re-exported from Data.Lens.Internal.Stall:

#### `Stall`

``` purescript
data Stall a b s t
  = Stall (s -> b -> t) (s -> Either t a)
```

The `Stall` profunctor characterizes an `AffineTraversal`.

##### Instances
``` purescript
Functor (Stall a b s)
Profunctor (Stall a b)
Strong (Stall a b)
Choice (Stall a b)
```

### Re-exported from Data.Lens.Internal.Tagged:

#### `Tagged`

``` purescript
newtype Tagged a b
  = Tagged b
```

##### Instances
``` purescript
Newtype (Tagged a b) _
(Eq b) => Eq (Tagged a b)
Eq1 (Tagged a)
(Ord b) => Ord (Tagged a b)
Ord1 (Tagged a)
Functor (Tagged a)
Profunctor Tagged
Choice Tagged
Costrong Tagged
Closed Tagged
Foldable (Tagged a)
Traversable (Tagged a)
```

### Re-exported from Data.Lens.Internal.Wander:

#### `Wander`

``` purescript
class (Strong p, Choice p) <= Wander p  where
  wander :: forall s t a b. (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
```

Class for profunctors that support polymorphic traversals.

##### Instances
``` purescript
Wander Function
(Applicative f) => Wander (Star f)
```

