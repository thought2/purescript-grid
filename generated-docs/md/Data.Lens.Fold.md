## Module Data.Lens.Fold

This module defines functions for working with folds.

#### `(^?)`

``` purescript
infixl 8 previewOn as ^?
```

#### `previewOn`

``` purescript
previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
```

Synonym for `preview`, flipped.

#### `(^..)`

``` purescript
infixl 8 toListOfOn as ^..
```

#### `toListOfOn`

``` purescript
toListOfOn :: forall s t a b. s -> Fold (Endo Function (List a)) s t a b -> List a
```

Synonym for `toListOf`, reversed.

#### `preview`

``` purescript
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
```

Previews the first value of a fold, if there is any.

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

#### `toListOf`

``` purescript
toListOf :: forall s t a b. Fold (Endo Function (List a)) s t a b -> s -> List a
```

Collects the foci of a `Fold` into a list.

#### `firstOf`

``` purescript
firstOf :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
```

The first focus of a `Fold`, if there is any. Synonym for `preview`.

#### `lastOf`

``` purescript
lastOf :: forall s t a b. Fold (Last a) s t a b -> s -> Maybe a
```

The last focus of a `Fold`, if there is any.

#### `maximumOf`

``` purescript
maximumOf :: forall s t a b. Ord a => Fold (Endo Function (Maybe a)) s t a b -> s -> Maybe a
```

The maximum of all foci of a `Fold`, if there is any.

#### `minimumOf`

``` purescript
minimumOf :: forall s t a b. Ord a => Fold (Endo Function (Maybe a)) s t a b -> s -> Maybe a
```

The minimum of all foci of a `Fold`, if there is any.

#### `allOf`

``` purescript
allOf :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> (a -> r) -> s -> r
```

Whether all foci of a `Fold` satisfy a predicate.

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

#### `orOf`

``` purescript
orOf :: forall s t a b. HeytingAlgebra a => Fold (Disj a) s t a b -> s -> a
```

The disjunction of all foci of a `Fold`.

#### `elemOf`

``` purescript
elemOf :: forall s t a b. Eq a => Fold (Disj Boolean) s t a b -> a -> s -> Boolean
```

Whether a `Fold` contains a given element.

#### `notElemOf`

``` purescript
notElemOf :: forall s t a b. Eq a => Fold (Conj Boolean) s t a b -> a -> s -> Boolean
```

Whether a `Fold` not contains a given element.

#### `sumOf`

``` purescript
sumOf :: forall s t a b. Semiring a => Fold (Additive a) s t a b -> s -> a
```

The sum of all foci of a `Fold`.

#### `productOf`

``` purescript
productOf :: forall s t a b. Semiring a => Fold (Multiplicative a) s t a b -> s -> a
```

The product of all foci of a `Fold`.

#### `lengthOf`

``` purescript
lengthOf :: forall s t a b. Fold (Additive Int) s t a b -> s -> Int
```

The number of foci of a `Fold`.

#### `findOf`

``` purescript
findOf :: forall s t a b. Fold (Endo Function (Maybe a)) s t a b -> (a -> Boolean) -> s -> Maybe a
```

Find the first focus of a `Fold` that satisfies a predicate, if there is any.

#### `sequenceOf_`

``` purescript
sequenceOf_ :: forall f s t a b. Applicative f => Fold (Endo Function (f Unit)) s t (f a) b -> s -> f Unit
```

Sequence the foci of a `Fold`, pulling out an `Applicative`, and ignore
the result. If you need the result, see `sequenceOf` for `Traversal`s.

#### `traverseOf_`

``` purescript
traverseOf_ :: forall f s t a b r. Applicative f => Fold (Endo Function (f Unit)) s t a b -> (a -> f r) -> s -> f Unit
```

Traverse the foci of a `Fold`, discarding the results.

#### `has`

``` purescript
has :: forall s t a b r. HeytingAlgebra r => Fold (Disj r) s t a b -> s -> r
```

Determines whether a `Fold` has at least one focus.

#### `hasn't`

``` purescript
hasn't :: forall s t a b r. HeytingAlgebra r => Fold (Conj r) s t a b -> s -> r
```

Determines whether a `Fold` does not have a focus.

#### `replicated`

``` purescript
replicated :: forall a b t r. Monoid r => Int -> Fold r a b a t
```

Replicates the elements of a fold.

#### `filtered`

``` purescript
filtered :: forall p a. Choice p => (a -> Boolean) -> Optic' p a a
```

Filters on a predicate.

#### `folded`

``` purescript
folded :: forall g a b t r. Monoid r => Foldable g => Fold r (g a) b a t
```

Folds over a `Foldable` container.

#### `unfolded`

``` purescript
unfolded :: forall r s t a b. Monoid r => (s -> Maybe (Tuple a s)) -> Fold r s t a b
```

Builds a `Fold` using an unfold.

#### `toArrayOf`

``` purescript
toArrayOf :: forall s t a b. Fold (Endo Function (List a)) s t a b -> s -> Array a
```

Collects the foci of a `Fold` into an array.

#### `toArrayOfOn`

``` purescript
toArrayOfOn :: forall s t a b. s -> Fold (Endo Function (List a)) s t a b -> Array a
```

Synonym for `toArrayOf`, reversed.

#### `ifoldMapOf`

``` purescript
ifoldMapOf :: forall r i s t a b. IndexedFold r i s t a b -> (i -> a -> r) -> s -> r
```

Fold map over an `IndexedFold`.

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

#### `iallOf`

``` purescript
iallOf :: forall i s t a b r. HeytingAlgebra r => IndexedFold (Conj r) i s t a b -> (i -> a -> r) -> s -> r
```

Whether all foci of an `IndexedFold` satisfy a predicate.

#### `ianyOf`

``` purescript
ianyOf :: forall i s t a b r. HeytingAlgebra r => IndexedFold (Disj r) i s t a b -> (i -> a -> r) -> s -> r
```

Whether any focus of an `IndexedFold` satisfies a predicate.

#### `ifindOf`

``` purescript
ifindOf :: forall i s t a b. IndexedFold (Endo Function (Maybe a)) i s t a b -> (i -> a -> Boolean) -> s -> Maybe a
```

Find the first focus of an `IndexedFold` that satisfies a predicate, if
there is any.

#### `itoListOf`

``` purescript
itoListOf :: forall i s t a b. IndexedFold (Endo Function (List (Tuple i a))) i s t a b -> s -> List (Tuple i a)
```

Collects the foci of an `IndexedFold` into a list.

#### `itraverseOf_`

``` purescript
itraverseOf_ :: forall i f s t a b r. Applicative f => IndexedFold (Endo Function (f Unit)) i s t a b -> (i -> a -> f r) -> s -> f Unit
```

Traverse the foci of an `IndexedFold`, discarding the results.

#### `iforOf_`

``` purescript
iforOf_ :: forall i f s t a b r. Applicative f => IndexedFold (Endo Function (f Unit)) i s t a b -> s -> (i -> a -> f r) -> f Unit
```

Flipped version of `itraverseOf_`.


### Re-exported from Data.Lens.Types:

#### `Fold'`

``` purescript
type Fold' r s a = Fold r s s a a
```

#### `Fold`

``` purescript
type Fold r s t a b = Optic (Forget r) s t a b
```

A fold.

