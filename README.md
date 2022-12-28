# purescript-grid

## Example
```hs
module Test.Readme where
```
Imports
```hs
import Prelude

import Data.Grid (Grid, Size(..), Vec(..), Pos(..))
import Data.Grid as Grid
import Data.Maybe (Maybe)
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
gridC :: Grid Char
gridC = Grid.fromArraysConform
  [ [ 'a', 'b', 'c', 'x', 'x' ]
  , [ 'd', 'e', 'f' ]
  ]
```
We can prrof that in the REPL
