## Module Data.Lens.Common

This module defines common lenses and prisms.

#### `simple`

``` purescript
simple :: forall p s a. Optic' p s a -> Optic' p s a
```

This is useful for when you want to restrict the type of another optic.
For example, suppose you have the following declarations:
```purescript
newtype X = X Int
derive instance newtypeX :: Newtype X _
```

Attempting to view with the `_Newtype` optic:
```purescript
X 42 ^. _Newtype
```
Will result in a type error:
```
 The inferred type
   forall t3 t5. Newtype t3 t5 => Int
 has type variables which are not mentioned in the body of the type.
 Consider adding a type annotation.
```

However, if we apply the `simple` function:
```purescript
 X 42 ^. simple _Newtype
```
We get the expected result `42`.


### Re-exported from Data.Lens.Lens.Tuple:

#### `second`

``` purescript
second :: forall p a b c. Strong p => p b c -> p (Tuple a b) (Tuple a c)
```

#### `first`

``` purescript
first :: forall p a b c. Strong p => p a b -> p (Tuple a c) (Tuple b c)
```

#### `_2`

``` purescript
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
```

Lens for the second component of a `Tuple`.

#### `_1`

``` purescript
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
```

Lens for the first component of a `Tuple`.

### Re-exported from Data.Lens.Lens.Unit:

#### `united`

``` purescript
united :: forall a. Lens' a Unit
```

There is a `Unit` in everything.
```purescript
> view united [1,2,3]
unit
> over united (\a -> a :: Unit) [1,2,3]
[1 2 3]
```

### Re-exported from Data.Lens.Prism.Either:

#### `right`

``` purescript
right :: forall p a b c. Choice p => p b c -> p (Either a b) (Either a c)
```

#### `left`

``` purescript
left :: forall p a b c. Choice p => p a b -> p (Either a c) (Either b c)
```

#### `_Right`

``` purescript
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
```

Prism for the `Right` constructor of `Either`.

#### `_Left`

``` purescript
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
```

Prism for the `Left` constructor of `Either`.

### Re-exported from Data.Lens.Prism.Maybe:

#### `_Nothing`

``` purescript
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
```

Prism for the `Nothing` constructor of `Maybe`.

#### `_Just`

``` purescript
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
```

Prism for the `Just` constructor of `Maybe`.

