## Module Data.Lens.Internal.Forget

#### `Forget`

``` purescript
newtype Forget r a b
  = Forget (a -> r)
```

Profunctor that forgets the `b` value and returns (and accumulates) a
value of type `r`.

`Forget r` is isomorphic to `Star (Const r)`, but can be given a `Cochoice`
instance.

##### Instances
``` purescript
Newtype (Forget r a b) _
(Semigroup r) => Semigroup (Forget r a b)
(Monoid r) => Monoid (Forget r a b)
Profunctor (Forget r)
(Monoid r) => Choice (Forget r)
Strong (Forget r)
Cochoice (Forget r)
(Monoid r) => Wander (Forget r)
```


