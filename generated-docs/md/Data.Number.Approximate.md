## Module Data.Number.Approximate

This module defines functions for comparing numbers.

#### `Fraction`

``` purescript
newtype Fraction
  = Fraction Number
```

A newtype for (small) numbers, typically in the range *[0:1]*. It is used
as an argument for `eqRelative`.

#### `eqRelative`

``` purescript
eqRelative :: Fraction -> Number -> Number -> Boolean
```

Compare two `Number`s and return `true` if they are equal up to the
given *relative* error (`Fraction` parameter).

This comparison is scale-invariant, i.e. if `eqRelative frac x y`, then
`eqRelative frac (s * x) (s * y)` for a given scale factor `s > 0.0`
(unless one of x, y is exactly `0.0`).

Note that the relation that `eqRelative frac` induces on `Number` is
not an equivalence relation. It is reflexive and symmetric, but not
transitive.

Example:
``` purs
> (eqRelative (Fraction 0.01)) 133.7 133.0
true

> (eqRelative (Fraction 0.001)) 133.7 133.0
false

> (eqRelative (Fraction 0.01)) (0.1 + 0.2) 0.3
true
```

#### `eqApproximate`

``` purescript
eqApproximate :: Number -> Number -> Boolean
```

Test if two numbers are approximately equal, up to a relative difference
of one part in a million:
``` purs
eqApproximate = eqRelative (Fraction 1.0e-6)
```

Example
``` purs
> 0.1 + 0.2 == 0.3
false

> 0.1 + 0.2 ≅ 0.3
true
```

#### `(~=)`

``` purescript
infix 4 eqApproximate as ~=
```

#### `(≅)`

``` purescript
infix 4 eqApproximate as ≅
```

#### `neqApproximate`

``` purescript
neqApproximate :: Number -> Number -> Boolean
```

The complement of `eqApproximate`.

#### `(≇)`

``` purescript
infix 4 neqApproximate as ≇
```

#### `Tolerance`

``` purescript
newtype Tolerance
  = Tolerance Number
```

A newtype for (small) numbers. It is used as an argument for `eqAbsolute`.

#### `eqAbsolute`

``` purescript
eqAbsolute :: Tolerance -> Number -> Number -> Boolean
```

Compare two `Number`s and return `true` if they are equal up to the given
(absolute) tolerance value. Note that this type of comparison is *not*
scale-invariant. The relation induced by `(eqAbsolute (Tolerance eps))` is
symmetric and reflexive, but not transitive.

Example:
``` purs
> (eqAbsolute (Tolerance 1.0)) 133.7 133.0
true

> (eqAbsolute (Tolerance 0.1)) 133.7 133.0
false
```


