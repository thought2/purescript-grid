## Module Data.Vector3

- Types
  - [Vec](#t:Vec)

- Constructors
  - [vec](#v:vec)
  - [oneX](#v:oneX)
  - [oneY](#v:oneY)
  - [oneZ](#v:oneZ)

- Destructors
  - [unVec](#v:unVec)
  - [getX](#v:getX)
  - [getY](#v:getY)
  - [getZ](#v:getY)

- Vector Modifiers
  - [rotRight](#v:rotRight)
  - [rotLeft](#v:rotLeft)

- Componentwise Operations
  - [vdiv](#v:vdiv)
  - [vmod](#v:vmod)
  - [half](#v:half)
  - [twice](#v:twice)

- Component Modifiers
  - [setX](#v:setX)
  - [setY](#v:setY)
  - [setZ](#v:setZ)
  - [modifyX](#v:modifyX)
  - [modifyY](#v:modifyY)
  - [modifyZ](#v:modifyZ)

- Lens API
  - [_x](#v:_x)
  - [_y](#v:_y)
  - [_z](#v:_z)

#### `Vec`

``` purescript
data Vec a
  = Vec a a a
```

Polymorphic 3D vector

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
vec :: forall a. a -> a -> a -> Vec a
```

Creates a vector from two components

#### `oneX`

``` purescript
oneX :: forall a. Semiring a => Vec a
```

Vector with Y value `one` and other values `zero`.

In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`.

```
> oneX + oneY + oneZ == one
true
```

#### `oneY`

``` purescript
oneY :: forall a. Semiring a => Vec a
```

Vector with Z value `one` and other values `zero`.

In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`

```
> oneX + oneY + oneZ == one
true
```

#### `oneZ`

``` purescript
oneZ :: forall a. Semiring a => Vec a
```

Vector with X value `one` and other values `zero`.

In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`

```
> oneX + oneY + oneZ == one
true
```

#### `unVec`

``` purescript
unVec :: forall a z. (a -> a -> a -> z) -> Vec a -> z
```

Pattern match on a vector by providing a reducer function

```
> unVec (\x y z -> x <> y <> z) (Vec "1" "2" "3")
"123"
```

#### `getX`

``` purescript
getX :: forall a. Vec a -> a
```

Retrieves the X component of a vector

```
> getX (Vec 1 2 3)
1
```

#### `getY`

``` purescript
getY :: forall a. Vec a -> a
```

Retrieves the Y component of a vector

```
> getY (Vec 1 2 3)
2
```

#### `getZ`

``` purescript
getZ :: forall a. Vec a -> a
```

Retrieves the Z component of a vector

```
> getZ (Vec 1 2 3)
3
```

#### `rotRight`

``` purescript
rotRight :: forall a. Vec a -> Vec a
```

Rotates the components of the vector to the right

```
> rotRight (Vec 1 2 3)
Vec 3 1 2
```

#### `rotLeft`

``` purescript
rotLeft :: forall a. Vec a -> Vec a
```

Rotates the components of the vector to the left

```
> rotRight (Vec 1 2 3)
Vec 2 3 1
```

#### `vdiv`

``` purescript
vdiv :: forall a. EuclideanRing a => Vec a -> Vec a -> Vec a
```

Divides two vectors componentwise.
This exists because there cannot be an `EuclideanRing` instance for `Vec`

```
> vdiv (Vec 9 6 4) (Vec 3 2 4)
Vec 3 3 1
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
> mod (Vec 12 120 1200) (Vec 120 100 1000)
Vec 2 20 200
```

#### `half`

``` purescript
half :: forall a. EuclideanRing a => Vec a -> Vec a
```

Halves the amount of each component

```
> half (Vec 10 100 1000)
Vec 5 50 500
```

#### `twice`

``` purescript
twice :: forall a. EuclideanRing a => Vec a -> Vec a
```

Duplicates the amount of each component

```
> twice (Vec 10 100 1000)
Vec 20 200 2000
```

#### `setX`

``` purescript
setX :: forall a. a -> Vec a -> Vec a
```

Sets the X component of a vector

```
> setX "G" (Vec "A" "B" "C")
Vec "G" "B" "C"
```

#### `setY`

``` purescript
setY :: forall a. a -> Vec a -> Vec a
```

Sets the Y component of a vector

```
> setY "G" (Vec "A" "B" "C")
Vec "A" "G" "C"
```

#### `setZ`

``` purescript
setZ :: forall a. a -> Vec a -> Vec a
```

Sets the Z component of a vector

```
> setZ "G" (Vec "A" "B" "C")
Vec "A" "B" "G"
```

#### `modifyX`

``` purescript
modifyX :: forall a. (a -> a) -> Vec a -> Vec a
```

Modifies the X component of a vector

```
> modifyX (add 10) (Vec 3 4 2)
Vec 13 4 2
```

#### `modifyY`

``` purescript
modifyY :: forall a. (a -> a) -> Vec a -> Vec a
```

Modifies the Y component of a vector

```
> modifyY (add 10) (Vec 3 4 2)
Vec 3 14 2
```

#### `modifyZ`

``` purescript
modifyZ :: forall a. (a -> a) -> Vec a -> Vec a
```

Modifies the Z component of a vector

```
> modifyZ (add 10) (Vec 3 4 2)
Vec 3 4 20
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

#### `_z`

``` purescript
_z :: forall a. Lens' (Vec a) a
```

A Lens on the Z component of a vector


