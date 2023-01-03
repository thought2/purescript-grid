## Module Test.QuickCheck.Arbitrary

#### `Arbitrary`

``` purescript
class Arbitrary t  where
  arbitrary :: Gen t
```

The `Arbitrary` class represents those types whose values can be
_randomly-generated_.

`arbitrary` uses the `Gen` monad to express a random generator for
the type `t`. Combinators in the `Test.QuickCheck.Gen`
module can be used to construct random generators.

##### Instances
``` purescript
Arbitrary Boolean
Arbitrary Number
Arbitrary Int
Arbitrary String
Arbitrary NonEmptyString
Arbitrary Char
Arbitrary Unit
Arbitrary Ordering
(Arbitrary a) => Arbitrary (Array a)
(Arbitrary a) => Arbitrary (NonEmptyArray a)
(Coarbitrary a, Arbitrary b) => Arbitrary (a -> b)
(Arbitrary a) => Arbitrary (First a)
(Arbitrary a) => Arbitrary (Last a)
(Arbitrary a) => Arbitrary (Additive a)
(Arbitrary a) => Arbitrary (Multiplicative a)
(Arbitrary a) => Arbitrary (Conj a)
(Arbitrary a) => Arbitrary (Disj a)
(Arbitrary a) => Arbitrary (Dual a)
(Arbitrary (c a a)) => Arbitrary (Endo c a)
(Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
(Arbitrary a) => Arbitrary (Maybe a)
(Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
(Arbitrary a) => Arbitrary (List a)
(Arbitrary a) => Arbitrary (Identity a)
(Arbitrary a) => Arbitrary (Lazy a)
(Arbitrary (f a), Arbitrary a) => Arbitrary (NonEmpty f a)
(Arbitrary a) => Arbitrary (NonEmptyList a)
Arbitrary NoArguments
(Arbitrary l, ArbitraryGenericSum r) => Arbitrary (Sum l r)
(Arbitrary l, Arbitrary r) => Arbitrary (Product l r)
(Arbitrary a) => Arbitrary (Constructor s a)
(Arbitrary a) => Arbitrary (Argument a)
(RowToList row list, ArbitraryRowList list row) => Arbitrary (Record row)
```

#### `Coarbitrary`

``` purescript
class Coarbitrary t  where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```

The `Coarbitrary` class represents types which appear on the left of
an `Arbitrary` function arrow.

To construct an `Arbitrary` instance for the type `a -> b`, we need to
use the input of type `a` to _perturb_ a random generator for `b`. This
is the role of the `coarbitrary` function.

`Coarbitrary` instances can be written using the `perturbGen` function.

##### Instances
``` purescript
Coarbitrary Boolean
Coarbitrary Number
Coarbitrary Int
Coarbitrary String
Coarbitrary NonEmptyString
Coarbitrary Char
Coarbitrary Unit
Coarbitrary Ordering
(Coarbitrary a) => Coarbitrary (Array a)
(Coarbitrary a) => Coarbitrary (NonEmptyArray a)
(Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b)
(Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b)
(Coarbitrary a) => Coarbitrary (Maybe a)
(Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b)
(Coarbitrary a) => Coarbitrary (List a)
(Coarbitrary a) => Coarbitrary (Identity a)
(Coarbitrary a) => Coarbitrary (Lazy a)
(Coarbitrary (f a), Coarbitrary a) => Coarbitrary (NonEmpty f a)
(Coarbitrary a) => Coarbitrary (NonEmptyList a)
Coarbitrary NoArguments
(Coarbitrary l, Coarbitrary r) => Coarbitrary (Sum l r)
(Coarbitrary l, Coarbitrary r) => Coarbitrary (Product l r)
(Coarbitrary a) => Coarbitrary (Constructor s a)
(Coarbitrary a) => Coarbitrary (Argument a)
```

#### `genericArbitrary`

``` purescript
genericArbitrary :: forall a rep. Generic a rep => Arbitrary rep => Gen a
```

A `Generic` implementation of the `arbitrary` member from the `Arbitrary` type class.

#### `genericCoarbitrary`

``` purescript
genericCoarbitrary :: forall a rep t. Generic a rep => Coarbitrary rep => a -> Gen t -> Gen t
```

A `Generic` implementation of the `coarbitrary` member from the `Coarbitrary` type class.

#### `ArbitraryGenericSum`

``` purescript
class ArbitraryGenericSum t  where
  arbitraryGenericSum :: Array (Gen t)
```

To be able to evenly distribute over chains of Sum types we build up
a collection of generators and choose between.  Each right component
of a Sum is either a Constructor or another Sum.

##### Instances
``` purescript
(Arbitrary l, ArbitraryGenericSum r) => ArbitraryGenericSum (Sum l r)
(Arbitrary a) => ArbitraryGenericSum (Constructor s a)
```

#### `ArbitraryRowList`

``` purescript
class ArbitraryRowList list row | list -> row where
  arbitraryRecord :: Proxy list -> Gen (Record row)
```

A helper typeclass to implement `Arbitrary` for records.

##### Instances
``` purescript
ArbitraryRowList Nil ()
(Arbitrary a, ArbitraryRowList listRest rowRest, Lacks key rowRest, Cons key a rowRest rowFull, RowToList rowFull (Cons key a listRest), IsSymbol key) => ArbitraryRowList (Cons key a listRest) rowFull
```


