## Module Effect.Random

#### `random`

``` purescript
random :: Effect Number
```

Returns a random number between 0 (inclusive) and 1 (exclusive). This is
a direct wrapper around JavaScript's `Math.random()`.

#### `randomInt`

``` purescript
randomInt :: Int -> Int -> Effect Int
```

Takes a range specified by `low` (the first argument) and `high` (the
second), and returns a random integer uniformly distributed in the closed
interval `[low, high]`. It is unspecified what happens if `low > high`,
or if either of `low` or `high` is not an integer.

For example:
``` purescript
randomInt 1 10 >>= Console.print
```
will print a random integer between 1 and 10.

#### `randomRange`

``` purescript
randomRange :: Number -> Number -> Effect Number
```

Returns a random number between a minimum value (inclusive) and a maximum
value (exclusive). It is unspecified what happens if `maximum < minimum`.

For example:
``` purescript
randomRange 1.0 2.0 >>= Console.print
```
will print a random number between 1 and 2.

#### `randomBool`

``` purescript
randomBool :: Effect Boolean
```

Returns a random boolean value with an equal chance of being `true` or
`false`.


