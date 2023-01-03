## Module Data.Lens.Lens

This module defines functions for working with lenses.

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

#### `lens'`

``` purescript
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
```

#### `withLens`

``` purescript
withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
```

#### `cloneLens`

``` purescript
cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
```

#### `ilens`

``` purescript
ilens :: forall i s t a b. (s -> Tuple i a) -> (s -> b -> t) -> IndexedLens i s t a b
```

#### `ilens'`

``` purescript
ilens' :: forall i s t a b. (s -> Tuple (Tuple i a) (b -> t)) -> IndexedLens i s t a b
```

#### `withIndexedLens`

``` purescript
withIndexedLens :: forall i s t a b r. (AnIndexedLens i s t a b) -> ((s -> (Tuple i a)) -> (s -> b -> t) -> r) -> r
```

#### `cloneIndexedLens`

``` purescript
cloneIndexedLens :: forall i s t a b. AnIndexedLens i s t a b -> IndexedLens i s t a b
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


### Re-exported from Data.Lens.Types:

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

#### `IndexedLens'`

``` purescript
type IndexedLens' i s a = IndexedLens i s s a a
```

#### `IndexedLens`

``` purescript
type IndexedLens i s t a b = forall p. Strong p => IndexedOptic p i s t a b
```

An indexed lens.

#### `AnIndexedLens'`

``` purescript
type AnIndexedLens' i s a = AnIndexedLens i s s a a
```

#### `AnIndexedLens`

``` purescript
type AnIndexedLens i s t a b = IndexedOptic (Shop (Tuple i a) b) i s t a b
```

An indexed lens defined in terms of `Shop`, which can be used
to avoid issues with impredicativity.

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

