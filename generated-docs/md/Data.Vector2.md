## Module Data.Vector2

- Types
  - [Vec](#t:Vec)

- Constructors
  - [vec](#v:vec)
  - [oneX](#v:oneX)
  - [oneY](#v:oneY)

- Destructors
  - [unVec](#v:unVec)
  - [getX](#v:getX)
  - [getY](#v:getY)

- Vector Modifiers
  - [swap](#v:swap)

- Componentwise Operations
  - [vdiv](#v:vdiv)
  - [vmod](#v:vmod)
  - [half](#v:half)
  - [twice](#v:twice)

- Component Modifiers
  - [setX](#v:setX)
  - [setY](#v:setY)
  - [modifyX](#v:modifyX)
  - [modifyY](#v:modifyY)

- Lens API
  - [_x](#v:_x)
  - [_y](#v:_y)

#### `Vec`

``` purescript
data Vec a
  = Vec a a
```

Polymorphic 2D vector

##### Instances
``` purescript
Generic (Vec a) _
(Eq a) => Eq (Vec a)
(Ord a) => Ord (Vec a)
Functor Vec
Foldable Vec
Traversable Vec
(Show a) => Show (Vec a)
(Semiring a) => Semiring (Vec a)
(Ring a) => Ring (Vec a)
Applicative Vec
Apply Vec
```

#### `vec`

``` purescript
vec :: forall a. a -> a -> Vec a
```

Creates a vector from two components

#### `oneX`

``` purescript
oneX :: forall a. Semiring a => Vec a
```

Vector with X value `one` and Y value `zero`.

In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`

```
> oneX + oneY == one
true
```

#### `oneY`

``` purescript
oneY :: forall a. Semiring a => Vec a
```

Vector with X value `zero` and Y value `one`.

In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`.

```
> oneX + oneY == one
true
```

#### `unVec`

``` purescript
unVec :: forall a z. (a -> a -> z) -> Vec a -> z
```

Pattern match on a vector by providing a reducer function

```
> unVec (+) (Vec 1 2)
3
```

#### `getX`

``` purescript
getX :: forall a. Vec a -> a
```

Retrieves the X component of a vector

```
> getX (Vec 1 2)
1
```

#### `getY`

``` purescript
getY :: forall a. Vec a -> a
```

Retrieves the Y component of a vector

```
> getY (Vec 1 2)
2
```

#### `swap`

``` purescript
swap :: forall a. Vec a -> Vec a
```

Exchanges the X and Y component of a vector

```
> swap (Vec 1 2)
Vec 2 1
```

#### `vdiv`

``` purescript
vdiv :: forall a. EuclideanRing a => Vec a -> Vec a -> Vec a
```

Divides two vectors componentwise.
This exists because there cannot be an `EuclideanRing` instance for `Vec`

```
> vdiv (Vec 9 6) (Vec 3 2)
Vec 3 3
```

#### `(//)`

``` purescript
infixl 7 vdiv as //
```

#### `vmod`

``` purescript
vmod :: forall a. EuclideanRing a => Vec a -> Vec a -> Vec a
```

Componentwise Modulo operation
This exists because there cannot be an `EuclideanRing` instance for `Vec`

```
> mod (Vec 12 120) (Vec 120 100)
Vec 2 20
```

#### `half`

``` purescript
half :: forall a. EuclideanRing a => Vec a -> Vec a
```

Halves the amount of each component

```
> half (Vec 10 100)
Vec 5 50
```

#### `twice`

``` purescript
twice :: forall a. EuclideanRing a => Vec a -> Vec a
```

Duplicates the amount of each component

```
> twice (Vec 10 100)
Vec 20 200
```

#### `setX`

``` purescript
setX :: forall a. a -> Vec a -> Vec a
```

Sets the X component of a vector

```
> setX "C" (Vec "A" "B")
Vec "C" "B"
```

#### `setY`

``` purescript
setY :: forall a. a -> Vec a -> Vec a
```

Sets the Y component of a vector

```
> setY "C" (Vec "A" "B")
Vec "A" "C"
```

#### `modifyX`

``` purescript
modifyX :: forall a. (a -> a) -> Vec a -> Vec a
```

Modifies the X component of a vector

```
> modifyX (add 10) (Vec 3 4)
Vec 13 4
```

#### `modifyY`

``` purescript
modifyY :: forall a. (a -> a) -> Vec a -> Vec a
```

Modifies the Y component of a vector

```
> modifyY (add 10) (Vec 3 4)
Vec 3 14
```

#### `_x`

``` purescript
_x :: forall a. Lens' (Vec a) a
```

A Lens on the X component of a vector

#### `_y`

``` purescript
_y :: forall a. Lens' (Vec a) a
```

A Lens on the Y component of a vector


