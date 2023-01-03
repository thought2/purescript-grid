## Module Data.Lens

This module re-exports types and functions from other modules:

- [`module Data.Lens.Iso`](Lens/Iso.md)
- [`module Data.Lens.Lens`](Lens/Lens.md)
- [`module Data.Lens.Prism`](Lens/Prism.md)
- [`module Data.Lens.Traversal`](Lens/Traversal.md)
- [`module Data.Lens.Types`](Lens/Types.md)
- [`module Data.Lens.Setter`](Lens/Setter.md)
- [`module Data.Lens.Getter`](Lens/Getter.md)
- [`module Data.Lens.Fold`](Lens/Fold.md)
- [`module Data.Lens.Common`](Lens/Common.md)


### Re-exported from Data.Lens.Common:

#### `united`

``` purescript
united :: forall a. Lens' a Unit
```

There is a `Unit` in everything.
```purescript
> view united [1,2,3]
unit
> over united (\a -> a :: Unit) [1,2,3]
[1 2 3]
```

#### `second`

``` purescript
second :: forall p a b c. Strong p => p b c -> p (Tuple a b) (Tuple a c)
```

#### `right`

``` purescript
right :: forall p a b c. Choice p => p b c -> p (Either a b) (Either a c)
```

#### `left`

``` purescript
left :: forall p a b c. Choice p => p a b -> p (Either a c) (Either b c)
```

#### `first`

``` purescript
first :: forall p a b c. Strong p => p a b -> p (Tuple a c) (Tuple b c)
```

#### `_Right`

``` purescript
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
```

Prism for the `Right` constructor of `Either`.

#### `_Nothing`

``` purescript
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
```

Prism for the `Nothing` constructor of `Maybe`.

#### `_Left`

``` purescript
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
```

Prism for the `Left` constructor of `Either`.

#### `_Just`

``` purescript
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
```

Prism for the `Just` constructor of `Maybe`.

#### `_2`

``` purescript
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
```

Lens for the second component of a `Tuple`.

#### `_1`

``` purescript
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
```

Lens for the first component of a `Tuple`.

### Re-exported from Data.Lens.Fold:

#### `unfolded`

``` purescript
unfolded :: forall r s t a b. Monoid r => (s -> Maybe (Tuple a s)) -> Fold r s t a b
```

Builds a `Fold` using an unfold.

#### `toListOfOn`

``` purescript
toListOfOn :: forall s t a b. s -> Fold (Endo Function (List a)) s t a b -> List a
```

Synonym for `toListOf`, reversed.

#### `toListOf`

``` purescript
toListOf :: forall s t a b. Fold (Endo Function (List a)) s t a b -> s -> List a
```

Collects the foci of a `Fold` into a list.

#### `toArrayOfOn`

``` purescript
toArrayOfOn :: forall s t a b. s -> Fold (Endo Function (List a)) s t a b -> Array a
```

Synonym for `toArrayOf`, reversed.

#### `toArrayOf`

``` purescript
toArrayOf :: forall s t a b. Fold (Endo Function (List a)) s t a b -> s -> Array a
```

Collects the foci of a `Fold` into an array.

#### `sumOf`

``` purescript
sumOf :: forall s t a b. Semiring a => Fold (Additive a) s t a b -> s -> a
```

The sum of all foci of a `Fold`.

#### `sequenceOf_`

``` purescript
sequenceOf_ :: forall f s t a b. Applicative f => Fold (Endo Function (f Unit)) s t (f a) b -> s -> f Unit
```

Sequence the foci of a `Fold`, pulling out an `Applicative`, and ignore
the result. If you need the result, see `sequenceOf` for `Traversal`s.

#### `replicated`

``` purescript
replicated :: forall a b t r. Monoid r => Int -> Fold r a b a t
```

Replicates the elements of a fold.

#### `productOf`

``` purescript
productOf :: forall s t a b. Semiring a => Fold (Multiplicative a) s t a b -> s -> a
```

The product of all foci of a `Fold`.

#### `previewOn`

``` purescript
previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
```

Synonym for `preview`, flipped.

#### `preview`

``` purescript
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
```

Previews the first value of a fold, if there is any.

#### `orOf`

``` purescript
orOf :: forall s t a b. HeytingAlgebra a => Fold (Disj a) s t a b -> s -> a
```

The disjunction of all foci of a `Fold`.

#### `notElemOf`

``` purescript
notElemOf :: forall s t a b. Eq a => Fold (Conj Boolean) s t a b -> a -> s -> Boolean
```

Whether a `Fold` not contains a given element.

#### `minimumOf`

``` purescript
minimumOf :: forall s t a b. Ord a => Fold (Endo Function (Maybe a)) s t a b -> s -> Maybe a
```

The minimum of all foci of a `Fold`, if there is any.

#### `maximumOf`

``` purescript
maximumOf :: forall s t a b. Ord a => Fold (Endo Function (Maybe a)) s t a b -> s -> Maybe a
```

The maximum of all foci of a `Fold`, if there is any.

#### `lengthOf`

``` purescript
lengthOf :: forall s t a b. Fold (Additive Int) s t a b -> s -> Int
```

The number of foci of a `Fold`.

#### `lastOf`

``` purescript
lastOf :: forall s t a b. Fold (Last a) s t a b -> s -> Maybe a
```

The last focus of a `Fold`, if there is any.

#### `itraverseOf_`

``` purescript
itraverseOf_ :: forall i f s t a b r. Applicative f => IndexedFold (Endo Function (f Unit)) i s t a b -> (i -> a -> f r) -> s -> f Unit
```

Traverse the foci of an `IndexedFold`, discarding the results.

#### `itoListOf`

``` purescript
itoListOf :: forall i s t a b. IndexedFold (Endo Function (List (Tuple i a))) i s t a b -> s -> List (Tuple i a)
```

Collects the foci of an `IndexedFold` into a list.

#### `ifoldrOf`

``` purescript
ifoldrOf :: forall i s t a b r. IndexedFold (Endo Function r) i s t a b -> (i -> a -> r -> r) -> r -> s -> r
```

Right fold over an `IndexedFold`.

#### `ifoldlOf`

``` purescript
ifoldlOf :: forall i s t a b r. IndexedFold (Dual (Endo Function r)) i s t a b -> (i -> r -> a -> r) -> r -> s -> r
```

Left fold over an `IndexedFold`.

#### `ifoldMapOf`

``` purescript
ifoldMapOf :: forall r i s t a b. IndexedFold r i s t a b -> (i -> a -> r) -> s -> r
```

Fold map over an `IndexedFold`.

#### `ianyOf`

``` purescript
ianyOf :: forall i s t a b r. HeytingAlgebra r => IndexedFold (Disj r) i s t a b -> (i -> a -> r) -> s -> r
```

Whether any focus of an `IndexedFold` satisfies a predicate.

#### `iallOf`

``` purescript
iallOf :: forall i s t a b r. HeytingAlgebra r => IndexedFold (Conj r) i s t a b -> (i -> a -> r) -> s -> r
```

Whether all foci of an `IndexedFold` satisfy a predicate.

#### `hasn't`

``` purescript
hasn't :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> s -> r
```

Determines whether a `Fold` does not have a focus.

#### `has`

``` purescript
has :: forall s t a b r. HeytingAlgebra r => Fold (Disj r) s t a b -> s -> r
```

Determines whether a `Fold` has at least one focus.

#### `foldrOf`

``` purescript
foldrOf :: forall s t a b r. Fold (Endo Function r) s t a b -> (a -> r -> r) -> r -> s -> r
```

Right fold over a `Fold`.

#### `foldlOf`

``` purescript
foldlOf :: forall s t a b r. Fold (Dual (Endo Function r)) s t a b -> (r -> a -> r) -> r -> s -> r
```

Left fold over a `Fold`.

#### `folded`

``` purescript
folded :: forall g a b t r. Monoid r => Foldable g => Fold r (g a) b a t
```

Folds over a `Foldable` container.

#### `foldOf`

``` purescript
foldOf :: forall s t a b. Fold a s t a b -> s -> a
```

Folds all foci of a `Fold` to one. Note that this is the same as `view`.

#### `foldMapOf`

``` purescript
foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
```

Maps and then folds all foci of a `Fold`.

#### `firstOf`

``` purescript
firstOf :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
```

The first focus of a `Fold`, if there is any. Synonym for `preview`.

#### `findOf`

``` purescript
findOf :: forall s t a b. Fold (Endo Function (Maybe a)) s t a b -> (a -> Boolean) -> s -> Maybe a
```

Find the first focus of a `Fold` that satisfies a predicate, if there is any.

#### `filtered`

``` purescript
filtered :: forall p a. Choice p => (a -> Boolean) -> Optic' p a a
```

Filters on a predicate.

#### `elemOf`

``` purescript
elemOf :: forall s t a b. Eq a => Fold (Disj Boolean) s t a b -> a -> s -> Boolean
```

Whether a `Fold` contains a given element.

#### `anyOf`

``` purescript
anyOf :: forall s t a b r. HeytingAlgebra r => Fold (Disj r) s t a b -> (a -> r) -> s -> r
```

Whether any focus of a `Fold` satisfies a predicate.

#### `andOf`

``` purescript
andOf :: forall s t a b. HeytingAlgebra a => Fold (Conj a) s t a b -> s -> a
```

The conjunction of all foci of a `Fold`.

#### `allOf`

``` purescript
allOf :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> (a -> r) -> s -> r
```

Whether all foci of a `Fold` satisfy a predicate.

#### `(^?)`

``` purescript
infixl 8 previewOn as ^?
```

#### `(^..)`

``` purescript
infixl 8 toListOfOn as ^..
```

### Re-exported from Data.Lens.Getter:

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

#### `use`

``` purescript
use :: forall s t a b m. MonadState s m => Getter s t a b -> m a
```

View the focus of a `Getter` in the state of a monad.

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

#### `(^.)`

``` purescript
infixl 8 viewOn as ^.
```

### Re-exported from Data.Lens.Grate:

#### `Grate'`

``` purescript
type Grate' s a = Grate s s a a
```

#### `Grate`

``` purescript
type Grate s t a b = forall p. Closed p => Optic p s t a b
```

A grate (http://r6research.livejournal.com/28050.html)

#### `zipWithOf`

``` purescript
zipWithOf :: forall s t a b. Optic Zipping s t a b -> (a -> a -> b) -> s -> s -> t
```

#### `zipFWithOf`

``` purescript
zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
```

#### `collectOf`

``` purescript
collectOf :: forall f s t a b. Functor f => Optic (Costar f) s t a (f a) -> (b -> s) -> f b -> t
```

### Re-exported from Data.Lens.Iso:

#### `withIso`

``` purescript
withIso :: forall s t a b r. AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
```

Extracts the pair of morphisms from an isomorphism.

#### `under`

``` purescript
under :: forall s t a b. AnIso s t a b -> (t -> s) -> b -> a
```

#### `uncurried`

``` purescript
uncurried :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (Tuple a b -> c) (Tuple d e -> f)
```

#### `re`

``` purescript
re :: forall p s t a b. Optic (Re p a b) s t a b -> Optic p b a t s
```

Reverses an optic.

#### `non`

``` purescript
non :: forall a. Eq a => a -> Iso' (Maybe a) a
```

If `a1` is obtained from `a` by removing a single value, then
`Maybe a1` is isomorphic to `a`.

#### `iso`

``` purescript
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
```

Create an `Iso` from a pair of morphisms.

#### `flipped`

``` purescript
flipped :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
```

#### `curried`

``` purescript
curried :: forall a b c d e f. Iso (Tuple a b -> c) (Tuple d e -> f) (a -> b -> c) (d -> e -> f)
```

#### `cloneIso`

``` purescript
cloneIso :: forall s t a b. AnIso s t a b -> Iso s t a b
```

Extracts an `Iso` from `AnIso`.

#### `auf`

``` purescript
auf :: forall s t a b e r p. Profunctor p => AnIso s t a b -> (p r a -> e -> b) -> p r s -> e -> t
```

#### `au`

``` purescript
au :: forall s t a b e. AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
```

### Re-exported from Data.Lens.Lens:

#### `withLens`

``` purescript
withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
```

#### `lensStore`

``` purescript
lensStore :: forall s t a b. ALens s t a b -> s -> Tuple a (b -> t)
```

Converts a lens into the form that `lens'` accepts.

Can be useful when defining a lens where the focus appears under multiple
constructors of an algebraic data type.  This function would be called for
each case of the data type.

For example:

```
data LensStoreExample = LensStoreA Int | LensStoreB (Tuple Boolean Int)

lensStoreExampleInt :: Lens' LensStoreExample Int
lensStoreExampleInt = lens' case _ of
  LensStoreA i -> map LensStoreA <$> lensStore identity i
  LensStoreB i -> map LensStoreB <$> lensStore _2 i
```

#### `lens'`

``` purescript
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
```

#### `lens`

``` purescript
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
```

Create a `Lens` from a getter/setter pair.

```purescript
> species = lens _.species $ _ {species = _}
> view species {species : "bovine"}
"bovine"

> _2 = lens Tuple.snd $ \(Tuple keep _) new -> Tuple keep new
```

Note: `_2` is predefined in `Data.Lens.Tuple`.

#### `cloneLens`

``` purescript
cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
```

### Re-exported from Data.Lens.Prism:

#### `withPrism`

``` purescript
withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
```

#### `review`

``` purescript
review :: forall s t a b. Review s t a b -> b -> t
```

Create the "whole" corresponding to a specific "part":

```purescript
review solidFocus Color.white == Solid Color.white
```

#### `prism'`

``` purescript
prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
```

Create a `Prism` from a constructor and a matcher function that
produces a `Maybe`:

```purescript
solidFocus :: Prism' Fill Color
solidFocus = prism' Solid case _ of
  Solid color -> Just color
  _ -> Nothing
```

#### `prism`

``` purescript
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
```

Create a `Prism` from a constructor and a matcher function that
produces an `Either`:

```purescript
solidFocus :: Prism' Fill Color
solidFocus = prism Solid case _ of
  Solid color -> Right color
  anotherCase -> Left anotherCase
```

_Note_: The matcher function returns a result wrapped in `Either t`
to allow for type-changing prisms in the case where the input does
not match.

#### `only`

``` purescript
only :: forall a. Eq a => a -> Prism a a Unit Unit
```

`only` focuses not just on a case, but a specific value of that case.

```purescript
solidWhiteFocus :: Prism' Fill Unit
solidWhiteFocus = only $ Solid Color.white

is      solidWhiteFocus (Solid Color.white) == true
preview solidWhiteFocus (Solid Color.white) == Just unit
review  solidWhiteFocus unit                == Solid Color.white
```

*Note*: `only` depends on `Eq`. Strange definitions of `(==)`
(for example, that it counts any `Fill` as being equal to `Solid Color.white`)
will create a prism that violates the preview-review law. 

#### `nearly`

``` purescript
nearly :: forall a. a -> (a -> Boolean) -> Prism' a Unit
```

`nearly` is a variant of `only`. Like `only`, `nearly` produces
a prism that matches
a single value. Unlike `only`, it uses a predicate you supply
instead of depending on `class Eq`: 

```purescript
solidWhiteFocus :: Prism' Fill Unit
solidWhiteFocus = nearly (Solid Color.white) predicate
  where
    predicate candidate =
      color.toHexString == Color.white.toHexString
```

#### `matching`

``` purescript
matching :: forall s t a b. APrism s t a b -> s -> Either t a
```

#### `isn't`

``` purescript
isn't :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
```

Ask if `preview prism` would produce a `Nothing`.

#### `is`

``` purescript
is :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
```

Ask if `preview prism` would produce a `Just`.

#### `clonePrism`

``` purescript
clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
```

### Re-exported from Data.Lens.Setter:

#### `subOver`

``` purescript
subOver :: forall s t a. Ring a => Setter s t a a -> a -> s -> t
```

#### `subModifying`

``` purescript
subModifying :: forall s a m. MonadState s m => Ring a => Setter' s a -> a -> m Unit
```

#### `setJust`

``` purescript
setJust :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
```

#### `set`

``` purescript
set :: forall s t a b. Setter s t a b -> b -> s -> t
```

Set the foci of a `Setter` to a constant value.

#### `over`

``` purescript
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
```

Apply a function to the foci of a `Setter`.

#### `mulOver`

``` purescript
mulOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
```

#### `mulModifying`

``` purescript
mulModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
```

#### `modifying`

``` purescript
modifying :: forall s a b m. MonadState s m => Setter s s a b -> (a -> b) -> m Unit
```

Modify the foci of a `Setter` in a monadic state.

#### `iover`

``` purescript
iover :: forall i s t a b. IndexedSetter i s t a b -> (i -> a -> b) -> s -> t
```

Apply a function to the foci of a `Setter` that may vary with the index.

#### `divOver`

``` purescript
divOver :: forall s t a. EuclideanRing a => Setter s t a a -> a -> s -> t
```

#### `divModifying`

``` purescript
divModifying :: forall s a m. MonadState s m => EuclideanRing a => Setter' s a -> a -> m Unit
```

#### `disjOver`

``` purescript
disjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
```

#### `disjModifying`

``` purescript
disjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
```

#### `conjOver`

``` purescript
conjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
```

#### `conjModifying`

``` purescript
conjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
```

#### `assignJust`

``` purescript
assignJust :: forall s a b m. MonadState s m => Setter s s a (Maybe b) -> b -> m Unit
```

#### `assign`

``` purescript
assign :: forall s a b m. MonadState s m => Setter s s a b -> b -> m Unit
```

Set the foci of a `Setter` in a monadic state to a constant value.

#### `appendOver`

``` purescript
appendOver :: forall s t a. Semigroup a => Setter s t a a -> a -> s -> t
```

#### `appendModifying`

``` purescript
appendModifying :: forall s a m. MonadState s m => Semigroup a => Setter' s a -> a -> m Unit
```

#### `addOver`

``` purescript
addOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
```

#### `addModifying`

``` purescript
addModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
```

#### `(||~)`

``` purescript
infixr 4 disjOver as ||~
```

#### `(||=)`

``` purescript
infix 4 disjModifying as ||=
```

#### `(?~)`

``` purescript
infixr 4 setJust as ?~
```

#### `(?=)`

``` purescript
infix 4 assignJust as ?=
```

#### `(<>~)`

``` purescript
infixr 4 appendOver as <>~
```

#### `(<>=)`

``` purescript
infix 4 appendModifying as <>=
```

#### `(//~)`

``` purescript
infixr 4 divOver as //~
```

#### `(//=)`

``` purescript
infix 4 divModifying as //=
```

#### `(.~)`

``` purescript
infixr 4 set as .~
```

#### `(.=)`

``` purescript
infix 4 assign as .=
```

#### `(-~)`

``` purescript
infixr 4 subOver as -~
```

#### `(-=)`

``` purescript
infix 4 subModifying as -=
```

#### `(+~)`

``` purescript
infixr 4 addOver as +~
```

#### `(+=)`

``` purescript
infix 4 addModifying as +=
```

#### `(*~)`

``` purescript
infixr 4 mulOver as *~
```

#### `(*=)`

``` purescript
infix 4 mulModifying as *=
```

#### `(&&~)`

``` purescript
infixr 4 conjOver as &&~
```

#### `(&&=)`

``` purescript
infix 4 conjModifying as &&=
```

#### `(%~)`

``` purescript
infixr 4 over as %~
```

#### `(%=)`

``` purescript
infix 4 modifying as %=
```

### Re-exported from Data.Lens.Traversal:

#### `traversed`

``` purescript
traversed :: forall t a b. Traversable t => Traversal (t a) (t b) a b
```

A `Traversal` for the elements of a `Traversable` functor.

```purescript
over traversed negate [1, 2, 3] == [-1,-2,-3]
over traversed negate (Just 3) == Just -3
```

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

#### `itraverseOf`

``` purescript
itraverseOf :: forall f i s t a b. IndexedOptic (Star f) i s t a b -> (i -> a -> f b) -> s -> f t
```

Turn a pure profunctor `IndexedTraversal` into a `lens`-like `IndexedTraversal`.

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

#### `Setter'`

``` purescript
type Setter' s a = Setter s s a a
```

#### `Setter`

``` purescript
type Setter s t a b = Optic Function s t a b
```

A setter.

#### `Review'`

``` purescript
type Review' s a = Review s s a a
```

#### `Review`

``` purescript
type Review s t a b = Optic Tagged s t a b
```

A review.

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

#### `Prism'`

``` purescript
type Prism' s a = Prism s s a a
```

#### `Prism`

``` purescript
type Prism s t a b = forall p. Choice p => Optic p s t a b
```

A prism.

#### `Optic'`

``` purescript
type Optic' p s a = Optic p s s a a
```

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

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

#### `Iso'`

``` purescript
type Iso' s a = Iso s s a a
```

#### `Iso`

``` purescript
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

A generalized isomorphism.

#### `IndexedTraversal'`

``` purescript
type IndexedTraversal' i s a = IndexedTraversal i s s a a
```

#### `IndexedTraversal`

``` purescript
type IndexedTraversal i s t a b = forall p. Wander p => IndexedOptic p i s t a b
```

An indexed traversal.

#### `IndexedSetter'`

``` purescript
type IndexedSetter' i s a = IndexedSetter i s s a a
```

#### `IndexedSetter`

``` purescript
type IndexedSetter i s t a b = IndexedOptic Function i s t a b
```

An indexed setter.

#### `IndexedOptic'`

``` purescript
type IndexedOptic' p i s a = IndexedOptic p i s s a a
```

#### `IndexedOptic`

``` purescript
type IndexedOptic p i s t a b = Indexed p i a b -> p s t
```

An indexed optic.

#### `IndexedGetter'`

``` purescript
type IndexedGetter' i s a = IndexedGetter i s s a a
```

#### `IndexedGetter`

``` purescript
type IndexedGetter i s t a b = IndexedFold a i s t a b
```

An indexed getter.

#### `IndexedFold'`

``` purescript
type IndexedFold' r i s a = IndexedFold r i s s a a
```

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

#### `Getter'`

``` purescript
type Getter' s a = Getter s s a a
```

#### `Getter`

``` purescript
type Getter s t a b = forall r. Fold r s t a b
```

A getter.

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

#### `Fold'`

``` purescript
type Fold' r s a = Fold r s s a a
```

#### `Fold`

``` purescript
type Fold r s t a b = Optic (Forget r) s t a b
```

A fold.

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

#### `ATraversal'`

``` purescript
type ATraversal' s a = ATraversal s s a a
```

#### `ATraversal`

``` purescript
type ATraversal s t a b = Optic (Bazaar Function a b) s t a b
```

A traversal defined in terms of `Bazaar`, which can be used
to avoid issues with impredicativity.

#### `APrism'`

``` purescript
type APrism' s a = APrism s s a a
```

#### `APrism`

``` purescript
type APrism s t a b = Optic (Market a b) s t a b
```

A prism defined in terms of `Market` to be safe from impredicativity
issues in the type checker. See the `docs/` folder for a more detailed
explanation.

#### `ALens'`

``` purescript
type ALens' s a = ALens s s a a
```

#### `ALens`

``` purescript
type ALens s t a b = Optic (Shop a b) s t a b
```

A lens defined in terms of `Shop`, which can be used
to avoid issues with impredicativity.

#### `AGetter'`

``` purescript
type AGetter' s a = AGetter s s a a
```

#### `AGetter`

``` purescript
type AGetter s t a b = Fold a s t a b
```

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

