## Module Test.QuickCheck

This module is a partial port of the Haskell QuickCheck library.

QuickCheck provides a way to write _property-based_ tests.

The `Arbitrary` and `CoArbitrary` type classes allow us to create
random data with which we can run our tests. This module provides
instances of both classes for PureScript's core data structures,
as well as functions for writing new instances.

Test suites can use the `quickCheck` and `quickCheckPure` functions
to test properties.

For example:

```purescript
main = quickCheck \n -> n + 1 > n
```

#### `quickCheck`

``` purescript
quickCheck :: forall prop. Testable prop => prop -> Effect Unit
```

Test a property.

This function generates a new random seed, runs 100 tests and
prints the test results to the console.

#### `quickCheckGen`

``` purescript
quickCheckGen :: forall prop. Testable prop => Gen prop -> Effect Unit
```

A version of `quickCheck` with the property specialized to `Gen`.

The `quickCheckGen` variants are useful for writing property tests where a
`MonadGen` constraint (or QuickCheck's `Gen` directly) is being used,
rather than relying on `Arbitrary` instances. Especially useful for the
`MonadGen`-constrained properties as they will not infer correctly when
used with the `quickCheck` functions unless an explicit type annotation is
used.

#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. Testable prop => Int -> prop -> Effect Unit
```

A variant of the `quickCheck` function which accepts an extra parameter
representing the number of tests which should be run.

#### `quickCheckGen'`

``` purescript
quickCheckGen' :: forall prop. Testable prop => Int -> Gen prop -> Effect Unit
```

A version of `quickCheck'` with the property specialized to `Gen`.

#### `quickCheckWithSeed`

``` purescript
quickCheckWithSeed :: forall prop. Testable prop => Seed -> Int -> prop -> Effect Unit
```

A variant of the `quickCheck'` function that accepts a specific seed as
well as the number tests that should be run.

#### `quickCheckGenWithSeed`

``` purescript
quickCheckGenWithSeed :: forall prop. Testable prop => Seed -> Int -> Gen prop -> Effect Unit
```

A version of `quickCheckWithSeed` with the property specialized to `Gen`.

#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. Testable prop => Seed -> Int -> prop -> List Result
```

Test a property, returning all test results as a List.

The first argument is the _random seed_ to be passed to the random generator.
The second argument is the number of tests to run.

#### `quickCheckPure'`

``` purescript
quickCheckPure' :: forall prop. Testable prop => Seed -> Int -> prop -> List (Tuple Seed Result)
```

Test a property, returning all test results as a List, with the Seed that
was used for each result.

The first argument is the _random seed_ to be passed to the random generator.
The second argument is the number of tests to run.

#### `quickCheckGenPure`

``` purescript
quickCheckGenPure :: forall prop. Testable prop => Seed -> Int -> Gen prop -> List Result
```

A version of `quickCheckPure` with the property specialized to `Gen`.

#### `quickCheckGenPure'`

``` purescript
quickCheckGenPure' :: forall prop. Testable prop => Seed -> Int -> Gen prop -> List (Tuple Seed Result)
```

A version of `quickCheckPure'` with the property specialized to `Gen`.

#### `ResultSummary`

``` purescript
type ResultSummary = { failures :: List { index :: Int, message :: String, seed :: Seed }, successes :: Int, total :: Int }
```

A type used to summarise the results from `quickCheckPure'`

#### `checkResults`

``` purescript
checkResults :: List (Tuple Seed Result) -> ResultSummary
```

Processes the results from `quickCheckPure'` to produce a `ResultSummary`.

#### `printSummary`

``` purescript
printSummary :: ResultSummary -> String
```

Print a one-line summary in the form "x/y test(s) passed."

#### `Testable`

``` purescript
class Testable prop  where
  test :: prop -> Gen Result
```

The `Testable` class represents _testable properties_.

A testable property is a function of zero or more `Arbitrary` arguments,
returning a `Boolean` or `Result`.

Testable properties can be passed to the `quickCheck` function.

##### Instances
``` purescript
Testable Result
Testable Boolean
(Arbitrary t, Testable prop) => Testable (t -> prop)
(Testable prop) => Testable (Gen prop)
```

#### `Result`

``` purescript
data Result
  = Success
  | Failed String
```

The result of a test: success or failure (with an error message).

##### Instances
``` purescript
Testable Result
Show Result
```

#### `withHelp`

``` purescript
withHelp :: Boolean -> String -> Result
```

This operator attaches an error message to a failed test.

For example:

```purescript
test x = myProperty x <?> ("myProperty did not hold for " <> show x)
```

#### `(<?>)`

``` purescript
infix 2 withHelp as <?>
```

#### `assertEquals`

``` purescript
assertEquals :: forall a. Eq a => Show a => a -> a -> Result
```

Self-documenting equality assertion

#### `(===)`

``` purescript
infix 2 assertEquals as ===
```

#### `(==?)`

``` purescript
infix 2 assertEquals as ==?
```

#### `assertNotEquals`

``` purescript
assertNotEquals :: forall a. Eq a => Show a => a -> a -> Result
```

Self-documenting inequality assertion

#### `(/==)`

``` purescript
infix 2 assertNotEquals as /==
```

#### `(/=?)`

``` purescript
infix 2 assertNotEquals as /=?
```

#### `assertLessThan`

``` purescript
assertLessThan :: forall a. Ord a => Show a => a -> a -> Result
```

#### `(<?)`

``` purescript
infix 2 assertLessThan as <?
```

#### `assertLessThanEq`

``` purescript
assertLessThanEq :: forall a. Ord a => Show a => a -> a -> Result
```

#### `(<=?)`

``` purescript
infix 2 assertLessThanEq as <=?
```

#### `assertGreaterThan`

``` purescript
assertGreaterThan :: forall a. Ord a => Show a => a -> a -> Result
```

#### `(>?)`

``` purescript
infix 2 assertGreaterThan as >?
```

#### `assertGreaterThanEq`

``` purescript
assertGreaterThanEq :: forall a. Ord a => Show a => a -> a -> Result
```

#### `(>=?)`

``` purescript
infix 2 assertGreaterThanEq as >=?
```


### Re-exported from Random.LCG:

#### `Seed`

``` purescript
newtype Seed
```

A seed for the linear congruential generator. We omit a `Semiring`
instance because there is no `zero` value, as 0 is not an acceptable
seed for the generator.

##### Instances
``` purescript
Eq Seed
Ord Seed
Show Seed
```

#### `unSeed`

``` purescript
unSeed :: Seed -> Int
```

#### `randomSeed`

``` purescript
randomSeed :: Effect Seed
```

Create a random seed

#### `mkSeed`

``` purescript
mkSeed :: Int -> Seed
```

### Re-exported from Test.QuickCheck.Arbitrary:

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

