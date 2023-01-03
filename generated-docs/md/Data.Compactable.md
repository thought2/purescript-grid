## Module Data.Compactable

#### `Compactable`

``` purescript
class Compactable f  where
  compact :: forall a. f (Maybe a) -> f a
  separate :: forall l r. f (Either l r) -> { left :: f l, right :: f r }
```

`Compactable` represents data structures which can be _compacted_/_filtered_.
This is a generalization of catMaybes as a new function `compact`. `compact`
has relations with `Functor`, `Applicative`, `Monad`, `Plus`, and `Traversable`
in that we can use these classes to provide the ability to operate on a data type
by eliminating intermediate Nothings. This is useful for representing the
filtering out of values, or failure.

To be compactable alone, no laws must be satisfied other than the type signature.

If the data type is also a Functor the following should hold:

- Functor Identity: `compact <<< map Just ≡ id`

According to Kmett, (Compactable f, Functor f) is a functor from the
kleisli category of Maybe to the category of Hask.
`Kleisli Maybe -> Hask`.

If the data type is also `Applicative` the following should hold:

- `compact <<< (pure Just <*> _) ≡ id`
- `applyMaybe (pure Just) ≡ id`
- `compact ≡ applyMaybe (pure id)`

If the data type is also a `Monad` the following should hold:

- `flip bindMaybe (pure <<< Just) ≡ id`
- `compact <<< (pure <<< (Just (=<<))) ≡ id`
- `compact ≡ flip bindMaybe pure`

If the data type is also `Plus` the following should hold:

- `compact empty ≡ empty`
- `compact (const Nothing <$> xs) ≡ empty`

##### Instances
``` purescript
Compactable Maybe
(Monoid m) => Compactable (Either m)
Compactable Array
Compactable List
(Ord k) => Compactable (Map k)
```

#### `compactDefault`

``` purescript
compactDefault :: forall f a. Functor f => Compactable f => f (Maybe a) -> f a
```

#### `separateDefault`

``` purescript
separateDefault :: forall f l r. Functor f => Compactable f => f (Either l r) -> { left :: f l, right :: f r }
```

#### `applyMaybe`

``` purescript
applyMaybe :: forall f a b. Apply f => Compactable f => f (a -> Maybe b) -> f a -> f b
```

#### `applyEither`

``` purescript
applyEither :: forall f a l r. Apply f => Compactable f => f (a -> Either l r) -> f a -> { left :: f l, right :: f r }
```

#### `bindMaybe`

``` purescript
bindMaybe :: forall m a b. Bind m => Compactable m => m a -> (a -> m (Maybe b)) -> m b
```

#### `bindEither`

``` purescript
bindEither :: forall m a l r. Bind m => Compactable m => m a -> (a -> m (Either l r)) -> { left :: m l, right :: m r }
```


