## Module Test.Data.Grid

#### `Any`

``` purescript
newtype Any a
  = Any a
```

##### Instances
``` purescript
Generic (Any a) _
Newtype (Any a) _
(Eq a) => Eq (Any a)
(Show a) => Show (Any a)
Arbitrary (Any Size)
Arbitrary (Any Array2d)
Arbitrary (Any Entries)
```

#### `Array2d`

``` purescript
newtype Array2d
  = Array2d (Array (Array Int))
```

##### Instances
``` purescript
Arbitrary (Invalid Array2d)
Arbitrary (Invalid (Tuple (Valid Size) (Valid Array2d)))
Arbitrary (Valid Array2d)
Arbitrary (Valid (Tuple Size Array2d))
Arbitrary (Any Array2d)
Generic Array2d _
Eq Array2d
Show Array2d
```

#### `Entries`

``` purescript
newtype Entries
  = Entries (Array (Tuple Pos Int))
```

##### Instances
``` purescript
Arbitrary (Invalid (Tuple (Valid Size) (Valid Entries)))
Arbitrary (Invalid Entries)
Arbitrary (Valid Entries)
Arbitrary (Valid (Tuple Size Entries))
Arbitrary (Any Entries)
Generic Entries _
Eq Entries
Show Entries
```

#### `Invalid`

``` purescript
newtype Invalid a
  = Invalid a
```

##### Instances
``` purescript
Generic (Invalid a) _
Newtype (Invalid a) _
(Eq a) => Eq (Invalid a)
(Show a) => Show (Invalid a)
Arbitrary (Invalid Size)
Arbitrary (Invalid Pos)
Arbitrary (Invalid Array2d)
Arbitrary (Invalid (Tuple (Valid Size) (Valid Array2d)))
Arbitrary (Invalid (Tuple (Valid Size) (Valid Entries)))
Arbitrary (Invalid Entries)
```

#### `Valid`

``` purescript
newtype Valid a
  = Valid a
```

##### Instances
``` purescript
Arbitrary (Invalid (Tuple (Valid Size) (Valid Array2d)))
Arbitrary (Invalid (Tuple (Valid Size) (Valid Entries)))
Generic (Valid a) _
Newtype (Valid a) _
(Eq a) => Eq (Valid a)
(Show a) => Show (Valid a)
Arbitrary (Valid Size)
Arbitrary (Valid Pos)
Arbitrary (Valid Array2d)
Arbitrary (Valid (Tuple Size Array2d))
Arbitrary (Valid Entries)
Arbitrary (Valid (Tuple Size Entries))
```

#### `genPosInt0`

``` purescript
genPosInt0 :: Gen Int
```

#### `spec`

``` purescript
spec :: Spec Unit
```


