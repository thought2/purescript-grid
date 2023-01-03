## Module Data.Lens.Record

#### `prop`

``` purescript
prop :: forall l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => Proxy l -> Lens (Record r1) (Record r2) a b
```

Construct a (type-changing) lens for a record property, by providing a
proxy for the `Symbol` which corresponds to the property label.

The lens is polymorphic in the rest of the row of property labels.

For example:

```purescript
prop (Proxy :: Proxy "foo")
  :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
```


