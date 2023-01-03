## Module Data.Grid

This module provides a type for 2D grids of variable sizes as well as some
functions for creating and manipulating values of this type. 
- Types
  - [Grid](#t:Grid)
  - [Pos](#t:Pos)
  - [Size](#t:Size)

- Constructors
  - [empty](#v:empty)
  - [singleton](#v:singleton)
  - [fill](#v:fill)
  - [fillFit](#v:fillFit)
  - [fromArrays](#v:fromArrays)
  - [fromArraysConform](#v:fromArraysConform)
  - [fromArraysPartial](#v:fromArraysPartial)
  - [mkGrid](#v:mkGrid)
  - [fromFoldable](#v:fromFoldable)
  - fromArray
  - fromArrayConform
  - fromArrayAsRow
  - fromArrayAsColumn
  - [genGrid](#v:genGrid)
  - [genGridFit](#v:genGridFit)

- Destructors
  - [toArrays](#v:toArrays)
  - [toUnfoldable](#v:toUnfoldable)
  - [size](#v:size)
  - [findEntry](#v:findEntry)
  - [getCell](#v:getCell)
  - [getCellModulo](#v:getCellModulo)
  - [positions](#v:positions)
  - getSubGrid
  - getSubGridModulo
  - getSubGridClip

- Grid Modifiers
  - [rotateClockwise](#v:rotateClockwise)
  - zipApply
  - zip
  - zipWithIndex
  - explodeApply
  - explode
  - explodeWithIndex
  - bind
  - bindWithIndex
  - join
  - [mirrorX](#v:mirrorX)
  - [mirrorY](#v:mirrorY)
  - [appendX](#:appendX)
  - [appendY](#:appendY)
  - resize
  - resizeFit
  - resizeTry

- SubGrid Modifiers
  - [setSubGrid](#v:setSubGrid)
  - [setSubGridClip](#v:setSubGridClip)
  - [setSubGridTry](#v:setSubGridTry)
  - [setSubGridModulo](#v:setSubGridModulo)
  - modifySubGrid
  - modifySubGridModulo
  - modifySubGridClip
  - modifySubGridTry

- Cell Modifiers
  - [setCell](#v:setCell) 
  - [setCellTry](#v:setCellTry)
  - [setCellModulo](#v:setCellModulo)
  - modifyCell
  - modifyCellModulo
  - modifyCellTry

- Pretty Printing
  - [CellFormatter](#t:CellFormatter)
  - [printGrid_](#v:printGrid_)
  - [printGrid](#v:printGrid)
  - [padRight](#v:padRight)
  - [padLeft](#v:padLeft)

#### `Grid`

``` purescript
data Grid a
```

2D grid with variable size

##### Instances
``` purescript
(Eq a) => Eq (Grid a)
Functor Grid
FunctorWithIndex Pos Grid
(Show a) => Show (Grid a)
Foldable Grid
FoldableWithIndex Pos Grid
Traversable Grid
TraversableWithIndex Pos Grid
(Arbitrary a) => Arbitrary (Grid a)
```

#### `Pos`

``` purescript
newtype Pos
  = Pos (Vec Int)
```

Position in a 2D Plane

##### Instances
``` purescript
FunctorWithIndex Pos Grid
FoldableWithIndex Pos Grid
TraversableWithIndex Pos Grid
Eq Pos
Ord Pos
Semiring Pos
Show Pos
Newtype Pos _
Generic Pos _
```

#### `Size`

``` purescript
newtype Size
  = Size (Vec Int)
```

Size in a 2D Plane

##### Instances
``` purescript
Eq Size
Show Size
Newtype Size _
Generic Size _
```

#### `empty`

``` purescript
empty :: forall a. Grid a
```

An empty grid of size `0|0`

```
> size empty
(Size (Vec 0 0))
```

#### `singleton`

``` purescript
singleton :: forall a. a -> Grid a
```

Creates a grid with one item of size `1|1`

```
> singleton 'A'
(mkGrid (Size (Vec 1 1)) [['A']])
```

#### `fill`

``` purescript
fill :: forall a. Size -> (Pos -> a) -> Maybe (Grid a)
```

Fills a grid of a given size with a function based on the position.
Only succeeds if the Size is valid. 

```
> fn (Pos (Vec x y)) = show x <> "-" <> show y
> fill (Size $ Vec 2 2) fn
(Just $ mkGrid (Size (Vec 2 2)) [["0-0","1-0"],["0-1","1-1"]])
```

#### `fillFit`

``` purescript
fillFit :: forall a. Size -> (Pos -> a) -> Grid a
```

Fills a grid of a given size with a function based on the position.
It the size is invalid an empty Grid is returned.

```
> fn (Pos (Vec x y)) = show x <> "-" <> show y
> fillFit (Size $ Vec 2 2) fn
(mkGrid (Size (Vec 2 2)) [["0-0","1-0"],["0-1","1-1"]])
```

#### `fromArrays`

``` purescript
fromArrays :: forall a. Size -> Array (Array a) -> Maybe (Grid a)
```

Creates a Grid of a given size from a nested Array.
Array structure must exactly match the size.

```
> fromArrays (Size $ Vec 2 2) [[1,2],[3,4]]
(Just (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]]))
```

#### `fromArraysConform`

``` purescript
fromArraysConform :: forall a. Array (Array a) -> Grid a
```

Creates a Grid from a nested Array.
The height is determined by the length of the outer Array.
The width by the shortest length of the inner Arrays.
With this rules there can exist a Grid for every input.

```
> fromArraysConform [[1,2],[3,4]]
(Just (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]]))
```

#### `fromArraysPartial`

``` purescript
fromArraysPartial :: forall a. Partial => Size -> Array (Array a) -> Grid a
```

Creates a Grid from a nested Array.
If the inner Arrays differ in length an exception is thrown.
Thus the Partial constraint.

```
> fromArraysPartial [[1,2],[3,4]]
(mkGrid (Size (Vec 2 2)) [[1,2],[3,4]])
```

#### `mkGrid`

``` purescript
mkGrid :: forall a. Partial => Size -> Array (Array a) -> Grid a
```

Alias for [fromArraysPartial](#v:fromArraysPartial)
Used in `Show` instance.

#### `fromFoldable`

``` purescript
fromFoldable :: forall a f. Foldable f => Size -> f (Tuple Pos a) -> Maybe (Grid a)
```

Creates a Grid from a Size and a Foldable containing entries for each cell.

```
> entries = [(Pos $ Vec 0 0) /\ 'A', (Pos $ Vec 0 1) /\ 'B']
> fromFoldable (Size $ Vec 1 2) entries
(Just (mkGrid (Size (Vec 1 2)) [['A'],['B']]))
```

#### `genGrid`

``` purescript
genGrid :: forall a. Size -> (Pos -> Gen a) -> Maybe (Gen (Grid a))
```

#### `genGridFit`

``` purescript
genGridFit :: forall a. Size -> (Pos -> Gen a) -> Gen (Grid a)
```

#### `toArrays`

``` purescript
toArrays :: forall a. Grid a -> Array (Array a)
```

Turns a Grid into nested Arrays.

```
> toArrays $ fromArraysConform [[1,2], [3,4]]
[[1,2],[3,4]]
```

#### `toUnfoldable`

``` purescript
toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
```

Turns a Grid into any Unfoldable containing entries for each cell.

```
> grid = fromArraysConform [[1], [2]]
> (toUnfoldable grid) :: Array _    
[(Tuple (Pos (Vec 0 0)) 1),(Tuple (Pos (Vec 0 1)) 2)]
```

#### `size`

``` purescript
size :: forall a. Grid a -> Size
```

Gets the size (width and height) of a Grid

```
> size $ fromArraysConform [[1,2], [3,4]]                     
(Size (Vec 2 2))
```

#### `findEntry`

``` purescript
findEntry :: forall a. (Pos -> a -> Boolean) -> Grid a -> Maybe (Tuple Pos a)
```

Finds an entry in a Grid.

```
> grid = fromArraysConform [[1,2], [3,4]]
> findEntry (\_ x -> x >= 2) grid
(Just (Tuple (Pos (Vec 0 1)) 3))
```

#### `getCell`

``` purescript
getCell :: forall a. Pos -> Grid a -> Maybe a
```

```
> grid = fromArraysConform [['a','b'], ['c','d']]
> getCell (Pos $ Vec 0 0) grid
(Just 'a')
```

#### `getCellModulo`

``` purescript
getCellModulo :: forall a. Pos -> Grid a -> a
```

```
> grid = fromArraysConform [['a','b'], ['c','d']]
> getCellModulo (Pos $ Vec 3 4) grid
'b'
```

#### `positions`

``` purescript
positions :: forall a. Grid a -> Array Pos
```

Gets the positions of the grid in column first order

```
> positions $ fromArraysConform [['a'], ['b']]         
[(Pos (Vec 0 0)),(Pos (Vec 0 1))]
```

#### `rotateClockwise`

``` purescript
rotateClockwise :: forall a. Grid a -> Grid a
```

Rotate the Grid clockwise (90 degree).

```
> rotateClockwise $ fromArraysConform [[1,2], [3,4]]
(mkGrid (Size (Vec 2 2)) [[3,1],[4,2]]))
```

#### `setSubGrid`

``` purescript
setSubGrid :: forall a. Pos -> Grid a -> Grid a -> Maybe (Grid a)
```

#### `setSubGridClip`

``` purescript
setSubGridClip :: forall a. Pos -> Grid a -> Grid a -> Grid a
```

#### `setSubGridTry`

``` purescript
setSubGridTry :: forall a. Pos -> Grid a -> Grid a -> Grid a
```

#### `setSubGridModulo`

``` purescript
setSubGridModulo :: forall a. Pos -> Grid a -> Grid a -> Grid a
```

#### `mirrorX`

``` purescript
mirrorX :: forall a. Grid a -> Grid a
```

#### `mirrorY`

``` purescript
mirrorY :: forall a. Grid a -> Grid a
```

#### `appendX`

``` purescript
appendX :: forall a. Grid a -> Grid a -> Maybe (Grid a)
```

#### `appendY`

``` purescript
appendY :: forall a. Grid a -> Grid a -> Maybe (Grid a)
```

#### `setCell`

``` purescript
setCell :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
```

```
> grid = fromArraysConform [[1,2], [3,4]]
> setCell (Pos $ Vec 0 0) 9 grid
(Just (mkGrid (Size (Vec 2 2)) [[9,2],[3,4]]))
```

#### `setCellTry`

``` purescript
setCellTry :: forall a. Pos -> a -> Grid a -> Grid a
```

```
> grid = fromArraysConform [[1,2], [3,4]]
> setCellTry (Pos $ Vec 0 0) 9 grid
(mkGrid (Size (Vec 2 2)) [[9,2],[3,4]])
```

#### `setCellModulo`

``` purescript
setCellModulo :: forall a. Pos -> a -> Grid a -> Grid a
```

#### `CellFormatter`

``` purescript
newtype CellFormatter
  = CellFormatter (Int -> String -> String)
```

##### Instances
``` purescript
Newtype CellFormatter _
```

#### `printGrid_`

``` purescript
printGrid_ :: Grid String -> String
```

Print a grid without options

```
grid = G.fromArraysConform
  [ [ "bird", "dog" ]
  , [ "cat", "horse" ]
  , [ "monkey", "giraffe" ]
  ]
```
---
```
> printGrid_ grid
bird    dog
cat     horse
monkey  giraffe
```

#### `PrintOpts`

``` purescript
type PrintOpts = { colSep :: String, formatCell :: CellFormatter, rowSep :: String }
```

Printing Options

 - formatCell formats the string in each cell
 - colSep separator between columns
 - rowSep separator between rows

#### `defaultPrintOpts`

``` purescript
defaultPrintOpts :: PrintOpts
```

Some defaults for `PrintOpts`

#### `printGrid`

``` purescript
printGrid :: PrintOpts -> Grid String -> String
```

Print a grid with options

```
grid = G.fromArraysConform
  [ [ "bird", "dog" ]
  , [ "cat", "horse" ]
  , [ "monkey", "giraffe" ]
  ]
```
---
```
> printGrid defaultPrintOpts { formatCell = padLeft '.' }
...bird ....dog
....cat ..horse
.monkey giraffe
```

#### `padRight`

``` purescript
padRight :: Char -> CellFormatter
```

`CellFormatter` that adds a padding to the right

```
> (unwrap $ G.padRight '.') 10 "Hello" 
Hello.....
```

#### `padLeft`

``` purescript
padLeft :: Char -> CellFormatter
```

`CellFormatter` that adds a padding to the left

```
> (unwrap $ G.padLeft '.') 10 "Hello"
.....Hello
```


### Re-exported from Data.Vector2:

#### `Vec`

``` purescript
data Vec a
  = Vec a a
```

Polymorphic 2D vector

##### Instances
``` purescript
Generic (Vec a) _
(Eq a) => Eq (Vec a)
(Ord a) => Ord (Vec a)
Functor Vec
Foldable Vec
Traversable Vec
(Show a) => Show (Vec a)
(Semiring a) => Semiring (Vec a)
(Ring a) => Ring (Vec a)
Applicative Vec
Apply Vec
```

