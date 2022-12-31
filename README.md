# purescript-grid

## Example
### Creating Grids
```hs
module Test.Readme where
```
Imports
```hs
import Prelude

import Control.Apply (lift2)
import Data.Grid (Grid, Size(..), Vec(..), Pos(..))
import Data.Grid as Grid
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Newtype as NT
import Data.Number (abs, acos, asin, floor)
import Data.Number as Num
import Data.String as Str
import Data.String.CodeUnits as StrC
import Data.Vector2 ((//))
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
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
Here we generate a Grid based on the position of each cell by using the `fill` function:
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

We can also customize the printing by overwriting the default options:

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

### Manipulating Grids

```hs
gridD :: Grid Char
gridD = Grid.fill (Size $ Vec 8 6) (\_ -> '*')
```
```text
> import Data.String.CodeUnits as StrC
> log $ printGrid_ $ StrC.singleton <$> gridD
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
```
```hs
gridE :: Grid Char
gridE = Grid.setCellTry (Pos $ Vec 2 3) 'A' gridD
```
```text
> log $ printGrid_ $ StrC.singleton <$> gridE
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * A * * * * *
* * * * * * * *
* * * * * * * *
```
```hs
gridF :: Grid Char
gridF = Grid.fromArraysConform
  [ [ 'H', 'E', 'L', 'L', 'O' ]
  , [ 'W', 'O', 'R', 'L', 'D' ]
  ]

gridG :: Grid Char
gridG = Grid.setSubGridTry (Pos $ Vec 1 2) gridF gridD
```
```text
> log $ printGrid_ $ StrC.singleton <$> gridG 
* * * * * * * *
* * * * * * * *
* H E L L O * *
* W O R L D * *
* * * * * * * *
* * * * * * * *
```
```hs
gridH :: Grid Char
gridH = Grid.setSubGridClip (Pos $ Vec 5 2) gridF gridD
```
```text
> log $ printGrid_ $ StrC.singleton <$> gridH
* * * * * * * *
* * * * * * * *
* * * * * H E L
* * * * * W O R
* * * * * * * *
* * * * * * * *
```
```hs
gridI :: Grid Char
gridI = Grid.setSubGridModulo (Pos $ Vec 5 2) gridF gridD
```
```text
> log $ printGrid_ $ StrC.singleton <$> gridI
* * * * * * * *
* * * * * * * *
L O * * * H E L
L D * * * W O R
* * * * * * * *
* * * * * * * *
```
```hs
norm :: Size -> Pos -> Vec Number
norm (Size vecSize) (Pos vecPos) =
  (toNumber <$> vecPos) // (toNumber <$> maxPos)
  where
  maxPos = vecSize - one

gridQuarterCircle :: Grid String
gridQuarterCircle = Grid.fill
  size
  (remap >>> drawCircle)

  where
  size = Size $ Vec 10 8
  remap = norm size

  drawCircle vec@(Vec x y) =
    let
      diff = acos x - asin y
    in
      if diff < 0.0 then
        "O"
      else if diff < 0.1 then
        "o"
      else "."
```
```text
> log $ Grid.printGrid_ gridQuarterCircle
. . . . . . . . . o
. . . . . . . . . O
. . . . . . . . . O
. . . . . . . . o O
. . . . . . . o O O
. . . . . . o O O O
. . . . o O O O O O
o O O O O O O O O O
```
```hs
gridHalfCircle :: Grid String
gridHalfCircle = Grid.mirrorY gridQuarterCircle `Grid.appendX` gridQuarterCircle
```
```text
> log $ Grid.printGrid_ $ gridHalfCircle
o . . . . . . . . . . . . . . . . . . o
O . . . . . . . . . . . . . . . . . . O
O . . . . . . . . . . . . . . . . . . O
O o . . . . . . . . . . . . . . . . o O
O O o . . . . . . . . . . . . . . o O O
O O O o . . . . . . . . . . . . o O O O
O O O O O o . . . . . . . . o O O O O O
O O O O O O O O O o o O O O O O O O O O
```
```hs
gridFullCircle :: Grid String
gridFullCircle = Grid.mirrorX gridHalfCircle `Grid.appendY` gridHalfCircle
```
```text
> log $ Grid.printGrid_ $ gridFullCircle 
O O O O O O O O O o o O O O O O O O O O
O O O O O o . . . . . . . . o O O O O O
O O O o . . . . . . . . . . . . o O O O
O O o . . . . . . . . . . . . . . o O O
O o . . . . . . . . . . . . . . . . o O
O . . . . . . . . . . . . . . . . . . O
O . . . . . . . . . . . . . . . . . . O
o . . . . . . . . . . . . . . . . . . o
o . . . . . . . . . . . . . . . . . . o
O . . . . . . . . . . . . . . . . . . O
O . . . . . . . . . . . . . . . . . . O
O o . . . . . . . . . . . . . . . . o O
O O o . . . . . . . . . . . . . . o O O
O O O o . . . . . . . . . . . . o O O O
O O O O O o . . . . . . . . o O O O O O
O O O O O O O O O o o O O O O O O O O O
```
```hs

```
[API Docs]: https://pursuit.purescript.org/packages/purescript-grid