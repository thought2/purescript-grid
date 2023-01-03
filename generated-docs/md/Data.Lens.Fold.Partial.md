## Module Data.Lens.Fold.Partial

#### `unsafeView`

``` purescript
unsafeView :: forall s t a b. Partial => s -> Fold (First a) s t a b -> a
```

#### `(^?!)`

``` purescript
infixl 8 unsafeView as ^?!
```

#### `unsafeIndexedFold`

``` purescript
unsafeIndexedFold :: forall i s t a b. Partial => s -> IndexedFold (First (Tuple i a)) i s t a b -> Tuple i a
```

#### `(^@?!)`

``` purescript
infixl 8 unsafeIndexedFold as ^@?!
```


