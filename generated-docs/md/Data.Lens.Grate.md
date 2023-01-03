## Module Data.Lens.Grate

This module defines functions for working with grates.

See <http://r6research.livejournal.com/28050.html>.

#### `grate`

``` purescript
grate :: forall s t a b. (((s -> a) -> b) -> t) -> Grate s t a b
```

#### `withGrate`

``` purescript
withGrate :: forall s t a b. AGrate s t a b -> ((s -> a) -> b) -> t
```

#### `cloneGrate`

``` purescript
cloneGrate :: forall s t a b. AGrate s t a b -> Grate s t a b
```

#### `cotraversed`

``` purescript
cotraversed :: forall f a b. Distributive f => Grate (f a) (f b) a b
```

#### `zipWithOf`

``` purescript
zipWithOf :: forall s t a b. Optic Zipping s t a b -> (a -> a -> b) -> s -> s -> t
```

#### `zipFWithOf`

``` purescript
zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
```

#### `collectOf`

``` purescript
collectOf :: forall f s t a b. Functor f => Optic (Costar f) s t a (f a) -> (b -> s) -> f b -> t
```


### Re-exported from Data.Lens.Types:

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

#### `Grate'`

``` purescript
type Grate' s a = Grate s s a a
```

#### `Grate`

``` purescript
type Grate s t a b = forall p. Closed p => Optic p s t a b
```

A grate (http://r6research.livejournal.com/28050.html)

#### `AGrate`

``` purescript
type AGrate s t a b = Optic (Grating a b) s t a b
```

A grate defined in terms of `Grating`, which can be used
to avoid issues with impredicativity.

