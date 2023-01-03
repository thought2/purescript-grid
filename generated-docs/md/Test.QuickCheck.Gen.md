## Module Test.QuickCheck.Gen

This module defines the random generator monad used by the `Test.QuickCheck`
module, as well as helper functions for constructing random generators.

#### `Gen`

``` purescript
newtype Gen a
```

The random generator monad

`Gen` is a state monad which encodes a linear congruential generator.

##### Instances
``` purescript
Functor Gen
Apply Gen
Applicative Gen
Bind Gen
Monad Gen
Alt Gen
MonadRec Gen
Lazy (Gen a)
MonadGen Gen
```

#### `unGen`

``` purescript
unGen :: forall a. Gen a -> State GenState a
```

Exposes the underlying State implementation.

#### `GenState`

``` purescript
type GenState = { newSeed :: Seed, size :: Size }
```

The state of the random generator monad

#### `Size`

``` purescript
type Size = Int
```

Tests are parameterized by the `Size` of the randomly-generated data,
the meaning of which depends on the particular generator used.

#### `repeatable`

``` purescript
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
```

Create a random generator for a function type.

#### `stateful`

``` purescript
stateful :: forall a. (GenState -> Gen a) -> Gen a
```

Create a random generator which uses the generator state explicitly.

#### `variant`

``` purescript
variant :: forall a. Seed -> Gen a -> Gen a
```

Modify a random generator by setting a new random seed.

#### `suchThat`

``` purescript
suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
```

Ensure that a generator only produces values that match a predicate. If
the predicate always returns false the generator will loop forever.

#### `sized`

``` purescript
sized :: forall a. (Size -> Gen a) -> Gen a
```

Create a random generator which depends on the size parameter.

#### `resize`

``` purescript
resize :: forall a. Size -> Gen a -> Gen a
```

Modify a random generator by setting a new size parameter.

#### `choose`

``` purescript
choose :: Number -> Number -> Gen Number
```

Create a random generator which samples a range of `Number`s i
with uniform probability.

#### `chooseInt`

``` purescript
chooseInt :: Int -> Int -> Gen Int
```

Create a random generator which chooses uniformly distributed
integers from the closed interval `[a, b]`.
Note that very large intervals will cause a loss of uniformity.

#### `oneOf`

``` purescript
oneOf :: forall a. NonEmptyArray (Gen a) -> Gen a
```

Create a random generator which selects and executes a random generator from
a non-empty array of random generators with uniform probability.

#### `frequency`

``` purescript
frequency :: forall a. NonEmptyArray (Tuple Number (Gen a)) -> Gen a
```

Create a random generator which selects and executes a random generator from
a non-empty, weighted list of random generators.

#### `arrayOf`

``` purescript
arrayOf :: forall a. Gen a -> Gen (Array a)
```

Create a random generator which generates an array of random values.

#### `arrayOf1`

``` purescript
arrayOf1 :: forall a. Gen a -> Gen (NonEmptyArray a)
```

Create a random generator which generates a non-empty array of random values.

#### `enum`

``` purescript
enum :: forall a. BoundedEnum a => Gen a
```

Create a random generator for a finite enumeration.
`toEnum i` must be well-behaved:
It must return a value wrapped in Just for all Ints between
`fromEnum bottom` and `fromEnum top`.

#### `listOf`

``` purescript
listOf :: forall a. Int -> Gen a -> Gen (List a)
```

Create a random generator which generates a list of random values of the specified size.

#### `vectorOf`

``` purescript
vectorOf :: forall a. Int -> Gen a -> Gen (Array a)
```

Create a random generator which generates a vector of random values of a specified size.

#### `elements`

``` purescript
elements :: forall a. NonEmptyArray a -> Gen a
```

Create a random generator which selects a value from a non-empty array with
uniform probability.

#### `shuffle`

``` purescript
shuffle :: forall a. Array a -> Gen (Array a)
```

Generate a random permutation of the given array

#### `runGen`

``` purescript
runGen :: forall a. Gen a -> GenState -> Tuple a GenState
```

Run a random generator

#### `evalGen`

``` purescript
evalGen :: forall a. Gen a -> GenState -> a
```

Run a random generator, keeping only the randomly-generated result

#### `perturbGen`

``` purescript
perturbGen :: forall a. Number -> Gen a -> Gen a
```

Perturb a random generator by modifying the current seed

#### `uniform`

``` purescript
uniform :: Gen Number
```

A random generator which approximates a uniform random variable on `[0, 1]`

#### `sample`

``` purescript
sample :: forall a. Seed -> Size -> Gen a -> Array a
```

Sample a random generator

#### `randomSample`

``` purescript
randomSample :: forall a. Gen a -> Effect (Array a)
```

Get a random sample of 10 values. For a single value, use `randomSampleOne`.

#### `randomSample'`

``` purescript
randomSample' :: forall a. Size -> Gen a -> Effect (Array a)
```

Sample a random generator, using a randomly generated seed

#### `randomSampleOne`

``` purescript
randomSampleOne :: forall a. Gen a -> Effect a
```

Generate a single value using a randomly generated seed.


