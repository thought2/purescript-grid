## Module Data.Lens.Internal.Bazaar

#### `Bazaar`

``` purescript
newtype Bazaar p a b s t
  = Bazaar (forall f. Applicative f => p a (f b) -> s -> f t)
```

This is used to characterize a Traversal.

##### Instances
``` purescript
Profunctor (Bazaar p a b)
Strong (Bazaar p a b)
Choice (Bazaar p a b)
Wander (Bazaar p a b)
```

#### `runBazaar`

``` purescript
runBazaar :: forall p a b s t. Bazaar p a b s t -> (forall f. Applicative f => p a (f b) -> s -> f t)
```


