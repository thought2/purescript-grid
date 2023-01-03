## Module Data.Lens.Prism

Prisms are used for selecting cases of a type, most often a sum
type. Consider this:

```purescript
data Fill -- think of a paint program filling a shape
  = NoFill
  | Solid Color
  | ...
```

A prism that focuses on `Solid` fills could be written like this:

```purescript
solidFocus :: Prism' Fill Color
solidFocus = prism' Solid case _ of
  Solid color -> Just color
  _ -> Nothing
```

... and used like this:

```purescript
preview solidFocus (Solid Color.white) == Just Color.white
preview solidFocus NoFill == Nothing

is solidFocus (Solid Color.white) == true
```

`review` can be used to go from a `Color` to a `Fill`:

```purescript
review solidFocus Color.white == Solid Color.white
```

For more information, see `PrismsForSumTypes.purs` in the
`examples/src` directory.

---------------

A well-behaved `Prism` will follow these laws:

**review-preview**: `preview` retrieves what `review` creates. Equationally:
  
```purescript
review prism >>> preview prism ≡ Just
```

An example:

```purescript
Color.white # review solidFocus # preview solidFocus
  == Just Color.white
```

**preview-review**: If `preview` retrieves something, `review` can create
the original from that something. Equationally:

```purescript
if preview prism s ≡ Just a then review prism a ≡ s
```

An example:

```purescript
Solid Color.white # preview solidFocus <#> review solidFocus
  == Solid Color.white
```

#### `prism'`

``` purescript
prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
```

Create a `Prism` from a constructor and a matcher function that
produces a `Maybe`:

```purescript
solidFocus :: Prism' Fill Color
solidFocus = prism' Solid case _ of
  Solid color -> Just color
  _ -> Nothing
```

#### `prism`

``` purescript
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
```

Create a `Prism` from a constructor and a matcher function that
produces an `Either`:

```purescript
solidFocus :: Prism' Fill Color
solidFocus = prism Solid case _ of
  Solid color -> Right color
  anotherCase -> Left anotherCase
```

_Note_: The matcher function returns a result wrapped in `Either t`
to allow for type-changing prisms in the case where the input does
not match.

#### `only`

``` purescript
only :: forall a. Eq a => a -> Prism a a Unit Unit
```

`only` focuses not just on a case, but a specific value of that case.

```purescript
solidWhiteFocus :: Prism' Fill Unit
solidWhiteFocus = only $ Solid Color.white

is      solidWhiteFocus (Solid Color.white) == true
preview solidWhiteFocus (Solid Color.white) == Just unit
review  solidWhiteFocus unit                == Solid Color.white
```

*Note*: `only` depends on `Eq`. Strange definitions of `(==)`
(for example, that it counts any `Fill` as being equal to `Solid Color.white`)
will create a prism that violates the preview-review law. 

#### `nearly`

``` purescript
nearly :: forall a. a -> (a -> Boolean) -> Prism' a Unit
```

`nearly` is a variant of `only`. Like `only`, `nearly` produces
a prism that matches
a single value. Unlike `only`, it uses a predicate you supply
instead of depending on `class Eq`: 

```purescript
solidWhiteFocus :: Prism' Fill Unit
solidWhiteFocus = nearly (Solid Color.white) predicate
  where
    predicate candidate =
      color.toHexString == Color.white.toHexString
```

#### `review`

``` purescript
review :: forall s t a b. Review s t a b -> b -> t
```

Create the "whole" corresponding to a specific "part":

```purescript
review solidFocus Color.white == Solid Color.white
```

#### `is`

``` purescript
is :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
```

Ask if `preview prism` would produce a `Just`.

#### `isn't`

``` purescript
isn't :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
```

Ask if `preview prism` would produce a `Nothing`.

#### `matching`

``` purescript
matching :: forall s t a b. APrism s t a b -> s -> Either t a
```

#### `clonePrism`

``` purescript
clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
```

#### `withPrism`

``` purescript
withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
```

#### `below`

``` purescript
below :: forall f s a. Traversable f => APrism' s a -> Prism' (f s) (f a)
```

`lift` a `Prism` through a `Traversable` functor, giving a `Prism` that matches 
only if all the elements of the container match the `Prism`.

``` purescript
>>> [Left 1, Right "foo", Left 4, Right "woot"]^..below _Right
[]
```

``` purescript
>>> [Right "hail hydra!", Right "foo", Right "blah", Right "woot"]^..below _Right
[["hail hydra!","foo","blah","woot"]]
```


### Re-exported from Data.Lens.Types:

#### `Review'`

``` purescript
type Review' s a = Review s s a a
```

#### `Review`

``` purescript
type Review s t a b = Optic Tagged s t a b
```

A review.

#### `Prism'`

``` purescript
type Prism' s a = Prism s s a a
```

#### `Prism`

``` purescript
type Prism s t a b = forall p. Choice p => Optic p s t a b
```

A prism.

#### `APrism'`

``` purescript
type APrism' s a = APrism s s a a
```

#### `APrism`

``` purescript
type APrism s t a b = Optic (Market a b) s t a b
```

A prism defined in terms of `Market` to be safe from impredicativity
issues in the type checker. See the `docs/` folder for a more detailed
explanation.

