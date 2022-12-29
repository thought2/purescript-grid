# purescript-grid

## Example
### Creating Grids
```hs
module Test.Readme where
```
Imports
```hs
import Prelude

import Data.Grid (Grid, Size(..), Vec(..), Pos(..))
import Data.Grid as Grid
import Data.Maybe (Maybe)
import Data.String as Str
import Effect.Console (log)
```
We can define a Grid from Arrays:
```hs
gridA :: Maybe (Grid Char)
gridA = Grid.fromArrays (Size $ Vec 3 2)
  [ [ 'a', 'b', 'c' ]
  , [ 'd', 'e', 'f' ]
  ]
```
This is a safe and explicit way to crate a Grid. As you can see, this
approach creates a `Maybe (Grid a)`. The obvious reason
is that the given Array structure does not match the expected size.  

For convenience you can alternatively use `fromArraysConform`.
```hs
gridB :: Grid Char
gridB = Grid.fromArraysConform
  [ [ 'a', 'b', 'c' ]
  , [ 'd', 'e', 'f' ]
  ]
```
This function does a bit more magic. You don't need to specify a size, it's derived
from the Arrays. In case one line is too short, other lines are cropped in
order to create a valid Grid.

So the following would result in the same Grid.
```hs
gridB' :: Grid Char
gridB' = Grid.fromArraysConform
  [ [ 'a', 'b', 'c', 'x', 'x' ]
  , [ 'd', 'e', 'f' ]
  ]
```
We can verify this in the REPL

```text
> gridB == gridC
true
```

There are many other ways to create a Grid. You can find them in the [API
Docs].

### Pretty Printing

When a value is returned from an expression in the REPL per default the
`Show` instance is used to log the value. In the case of a `Grid` this leads
the following rather technical representation. (`mkGrid` is another way to create a `Grid`, it has a `Partial` instance hence
it is ok that it does not return a `Maybe Grid`.)

```text
> gridB
(mkGrid (Size (Vec 3 2)) [['a','b','c'],['d','e','f']])
```

However, we can also use the `printGrid_` function to get a nicer output. The
function works on `Grid String` only, so we have to turn the characters into
Strings first by mapping over the grid:

```text
> log $ printGrid_ $ show <$> gridB
'a' 'b' 'c'
'd' 'e' 'f'
```

The printer also works if the strings in the cells have different lengths.
Here we generate a Grid with the `fill` function:
```hs
gridC :: Grid String
gridC = Grid.fill (Size $ Vec 4 5) fillFn
  where
  fillFn (Pos (Vec x y)) = Str.take (x * y + 1) "Abrakadabra"
```
And print it using the defaults:
```text
> log $ printGrid_ gridC
A           A           A           A          
A           Ab          Abr         Abra       
A           Abr         Abrak       Abrakad    
A           Abra        Abrakad     Abrakadabr 
A           Abrak       Abrakadab   Abrakadabra
```

We can also customize the printing by defining some parameters:

```hs
printOpts = Grid.defaultPrintOpts
  { formatCell = Grid.padLeft '.'
  , colSep = "  "
  }
```
And then pass them to `printGrid`: 
```text
> log $ printGrid printOpts gridC
..........A  ..........A  ..........A  ..........A
..........A  .........Ab  ........Abr  .......Abra
..........A  ........Abr  ......Abrak  ....Abrakad
..........A  .......Abra  ....Abrakad  .Abrakadabr
..........A  ......Abrak  ..Abrakadab  Abrakadabra
```

[API Docs]: https://pursuit.purescript.org/packages/purescript-grid
```hs

```
