## Module Random.LCG

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

#### `mkSeed`

``` purescript
mkSeed :: Int -> Seed
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

#### `lcgA`

``` purescript
lcgA :: Int
```

The *multiplier*: a magic constant for the linear congruential generator

#### `lcgC`

``` purescript
lcgC :: Int
```

The *increment*: a magic constant for the linear congruential generator

#### `lcgM`

``` purescript
lcgM :: Int
```

The *modulus*: a magic constant for the linear congruential generator.
It is equal to 2^31 - 1, a Mersenne prime. It is useful for this value to
be prime, because then the requirement of the initial seed being coprime
to the modulus is satisfied when the seed is between 1 and lcgM - 1.

#### `lcgNext`

``` purescript
lcgNext :: Seed -> Seed
```

Step the linear congruential generator

#### `lcgPerturb`

``` purescript
lcgPerturb :: Int -> Seed -> Seed
```

Perturb a seed value


