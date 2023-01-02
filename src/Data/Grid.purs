--------------------------------------------------------------------------------
--- Intro
--------------------------------------------------------------------------------

-- | Intro
-- |

--------------------------------------------------------------------------------
--- TOC
--------------------------------------------------------------------------------

-- | - Types
-- |   - [Grid](#t:Grid)
-- |   - [Pos](#t:Pos)
-- |   - [Size](#t:Size)
-- |
-- | - Constructors
-- |   - [empty](#v:empty)
-- |   - [singleton](#v:singleton)
-- |   - [fill](#v:fill)
-- |   - [fillTry](#v:fillTry)
-- |   - [fromArrays](#v:fromArrays)
-- |   - [fromArraysConform](#v:fromArraysConform)
-- |   - [fromArraysPartial](#v:fromArraysPartial)
-- |   - [mkGrid](#v:mkGrid)
-- |   - [fromFoldable](#v:fromFoldable)

-- |   - fromArray
-- |   - fromArrayConform
-- |   - fromArrayAsRow
-- |   - fromArrayAsColumn

-- |   - genGrid
-- |   - genGridSized
-- |
-- | - Destructors
-- |   - [toArrays](#v:toArrays)
-- |   - [toUnfoldable](#v:toUnfoldable)
-- |   - [size](#v:size)
-- |   - [findEntry](#v:findEntry)
-- |   - [getCell](#v:getCell)
-- |   - [getCellModulo](#v:getCellModulo)
-- |   - [positions](#v:positions)
-- |   - getSubGrid
-- |   - getSubGridModulo
-- |   - getSubGridClip
-- |
-- | - Grid Modifiers
-- |   - [rotateClockwise](#v:rotateClockwise)
-- |   - zipApply
-- |   - zip
-- |   - zipWithIndex
-- |   - explodeApply
-- |   - explode
-- |   - explodeWithIndex
-- |   - bind
-- |   - bindWithIndex
-- |   - join
-- |   - [mirrorX](#v:mirrorX)
-- |   - [mirrorY](#v:mirrorY)
-- |   - [appendX](#:appendX)
-- |   - [appendY](#:appendY)
-- |   - resize
-- |   - resizeFit
-- |   - resizeTry
-- |
-- | - SubGrid Modifiers
-- |   - [setSubGrid](#v:setSubGrid)
-- |   - [setSubGridClip](#v:setSubGridClip)
-- |   - [setSubGridTry](#v:setSubGridTry)
-- |   - [setSubGridModulo](#v:setSubGridModulo)
-- |   - modifySubGrid
-- |   - modifySubGridModulo
-- |   - modifySubGridClip
-- |   - modifySubGridTry
-- |
-- | - Cell Modifiers
-- |   - [setCell](#v:setCell) 
-- |   - [setCellTry](#v:setCellTry)
-- |   - [setCellModulo](#v:setCellModulo)
-- |   - modifyCell
-- |   - modifyCellModulo
-- |   - modifyCellTry
-- |
-- | - Pretty Printing
-- |   - [CellFormatter](#t:CellFormatter)
-- |   - [printGrid_](#v:printGrid_)
-- |   - [printGrid](#v:printGrid)
-- |   - [padRight](#v:padRight)
-- |   - [padLeft](#v:padLeft)

--------------------------------------------------------------------------------
--- Exports
--------------------------------------------------------------------------------

module Data.Grid
  -- Types
  ( Grid
  , Pos(..)
  , Size(..)

  -- Constructors
  , empty
  , singleton
  , fill
  , fillTry
  , fromArrays
  , fromArraysConform
  , fromArraysPartial
  , mkGrid
  , fromFoldable

  -- Destructors
  , toArrays
  , toUnfoldable
  , size
  , findEntry
  , getCell
  , getCellModulo
  , positions

  -- Grid Modifiers
  , rotateClockwise

  -- SubGrid Modifiers
  , setSubGrid
  , setSubGridClip
  , setSubGridTry
  , setSubGridModulo
  , mirrorX
  , mirrorY
  , appendX
  , appendY

  -- Cell Modifiers
  , setCell
  , setCellTry
  , setCellModulo

  -- Pretty Printing
  , CellFormatter(..)
  , printGrid_
  , PrintOpts
  , defaultPrintOpts
  , printGrid
  , padRight
  , padLeft

  , module Exp
  ) where

--------------------------------------------------------------------------------
--- Imports
--------------------------------------------------------------------------------

import Prelude

import Control.Alternative (guard)
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, and, fold, foldMap, foldl, foldr, length, traverse_)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as NT
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vector2 (Vec(..)) as Exp
import Data.Vector2 (Vec(..), oneX, oneY, vmod)
import Data.Vector2 as V2
import Data.Vector2 as Vec
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

-- | 2D grid with variable size

data Grid a = UnsafeGrid Size (Map Pos a)

-- | Equality of each cell
-- |
-- | ```
-- | > gridA = fromArraysConform [[1,2],[3,4]]
-- | > gridB = fromArraysConform [[1,2],[3,4]]  
-- | > gridA == gridB
-- | true
-- | ```

derive instance (Eq a) => Eq (Grid a)

-- | Maps over each cell
-- |
-- | ```
-- | > map (add 10) $ fromArraysConform [[1,2],[3,4]]
-- | (mkGrid (Size (Vec 2 2)) [[11,12],[13,14]])
-- | ```

derive instance Functor Grid

-- | Maps over each cell with position as index
-- |
-- | ```
-- | > grid = fromArraysConform [["A","B"],["C","D"]]
-- | > mapFn (Pos (Vec x y)) cell = show x <> show y <> cell
-- | > mapWithIndex mapFn grid
-- | (mkGrid (Size (Vec 2 2)) [["00A","10B"],["01C","11D"]])
-- | ```

instance FunctorWithIndex Pos Grid where
  mapWithIndex f (UnsafeGrid siz mp) = UnsafeGrid siz $ mapWithIndex f mp

-- | Show instance intended for debugging.
-- |
-- | ```
-- | > logShow $ fromArraysConform [[1,2],[3,4]]
-- | (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]])
-- | ```

instance Show a => Show (Grid a) where
  show grid = fold
    [ "("
    , "mkGrid"
    , " "
    , show $ size grid
    , " "
    , show $ toArrays grid
    , ")"
    ]

-- | Fold over each cell.
-- |
-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > foldr add 100 grid
-- | 110
-- | ```

instance Foldable Grid where
  foldr f x = gridGetMap >>> foldr f x
  foldl f x = gridGetMap >>> foldl f x
  foldMap f = gridGetMap >>> foldMap f

-- | Fold over each cell with positions as index.
-- |
-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > foldFn (Pos (Vec x y)) n acc = x + y + n + acc
-- | > foldrWithIndex foldFn 10 grid
-- | 24
-- | ```

instance FoldableWithIndex Pos Grid where
  foldrWithIndex f x = gridGetMap >>> foldrWithIndex f x
  foldlWithIndex f x = gridGetMap >>> foldlWithIndex f x
  foldMapWithIndex = foldMapWithIndexDefaultL

-- | ```
-- | > grid = fromArraysConform [[Just 1,Just 2],[Just 3,Just 4]]
-- | > sequence grid
-- | (Just (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]]))
-- | ```

instance Traversable Grid where
  traverse f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> traverse f oldMap
  sequence = sequenceDefault

-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > fn (Pos (Vec x _)) _ = Just x 
-- | > traverseWithIndex fn grid
-- | (Just (mkGrid (Size (Vec 2 2)) [[0,1],[0,1]]))
-- | ```

instance TraversableWithIndex Pos Grid where
  traverseWithIndex f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> (traverseWithIndex f oldMap)

-- | Position in a 2D Plane

newtype Pos = Pos (Vec Int)

derive instance Eq Pos

derive newtype instance Ord Pos

derive newtype instance Semiring Pos

instance Show Pos where
  show = genericShow

derive instance Newtype Pos _

derive instance Generic Pos _

-- | Size in a 2D Plane

newtype Size = Size (Vec Int)

derive instance Eq Size

instance Show Size where
  show = genericShow

derive instance Newtype Size _

derive instance Generic Size _

--------------------------------------------------------------------------------
--- Constructors
--------------------------------------------------------------------------------

-- | An empty grid of size `0|0`
-- |
-- | ```
-- | > size empty
-- | (Size (Vec 0 0))
-- | ```

empty :: forall a. Grid a
empty = UnsafeGrid (Size zero) Map.empty

-- | Creates a grid with one item of size `1|1`
-- |
-- | ```
-- | > singleton 'A'
-- | (mkGrid (Size (Vec 1 1)) [['A']])
-- | ```

singleton :: forall a. a -> Grid a
singleton x = UnsafeGrid (Size $ Vec 1 1) (Map.singleton (Pos $ Vec 0 0) x)

-- | Fills a grid of a given size with a function based on the position.
-- | Only succeeds if the Size is valid. 
-- |
-- | ```
-- | > fn (Pos (Vec x y)) = show x <> "-" <> show y
-- | > fill (Size $ Vec 2 2) fn
-- | (Just $ mkGrid (Size (Vec 2 2)) [["0-0","1-0"],["0-1","1-1"]])
-- | ```

fill :: forall a. Size -> (Pos -> a) -> Maybe (Grid a)
fill givenSize f = ado
  guard $ sizeIsValid givenSize
  let
    newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  in
    UnsafeGrid givenSize newMap
  where
  mkEntry k = Tuple k (f k)

-- | Fills a grid of a given size with a function based on the position.
-- | It the size is invalid an empty Grid is returned.
-- |
-- | ```
-- | > fn (Pos (Vec x y)) = show x <> "-" <> show y
-- | > fillTry (Size $ Vec 2 2) fn
-- | (mkGrid (Size (Vec 2 2)) [["0-0","1-0"],["0-1","1-1"]])
-- | ```

fillTry :: forall a. Size -> (Pos -> a) -> Grid a
fillTry unsafeSize f = UnsafeGrid givenSize newMap
  where
  givenSize = sizeNormalize unsafeSize
  newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  mkEntry k = Tuple k (f k)

-- | Creates a Grid of a given size from a nested Array.
-- | Array structure must exactly match the size.
-- |
-- | ```
-- | > fromArrays (Size $ Vec 2 2) [[1,2],[3,4]]
-- | (Just (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]]))
-- | ```

fromArrays :: forall a. Size -> Array (Array a) -> Maybe (Grid a)
fromArrays givenSize xs = do
  guard $ sizeIsValid givenSize
  guard (array2dMaxSize xs == givenSize)

  newMap <- positionsFromSize givenSize
    # traverse mkEntryM
    <#> Map.fromFoldable
  pure $
    UnsafeGrid givenSize newMap
  where
  mkEntryM pos = ado
    value <- array2dLookup pos xs
    in Tuple pos value

-- | Creates a Grid from a nested Array.
-- | The height is determined by the length of the outer Array.
-- | The width by the shortest length of the inner Arrays.
-- | With this rules there can exist a Grid for every input.
-- |
-- | ```
-- | > fromArraysConform [[1,2],[3,4]]
-- | (Just (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]]))
-- | ```

fromArraysConform :: forall a. Array (Array a) -> Grid a
fromArraysConform xs = UnsafeGrid newSize newMap
  where
  width = xs <#> Arr.length # Arr.sort # Arr.head # fromMaybe 0
  height = Arr.length xs
  newSize = sizeNormalize $ Size $ Vec width height

  newMap = positionsFromSize newSize
    <#> mkEntry
    # Map.fromFoldable

  mkEntry pos =
    let
      value = unsafePartial $ array2dLookupPartial "fromArraysConform" pos xs
    in
      Tuple pos value

-- | Creates a Grid from a nested Array.
-- | If the inner Arrays differ in length an exception is thrown.
-- | Thus the Partial constraint.
-- |
-- | ```
-- | > fromArraysPartial [[1,2],[3,4]]
-- | (mkGrid (Size (Vec 2 2)) [[1,2],[3,4]])
-- | ```

fromArraysPartial :: forall a. Partial => Size -> Array (Array a) -> Grid a
fromArraysPartial givenSize xs = case fromArrays givenSize xs of
  Just x -> x
  Nothing -> unsafeCrashWith "Arrays have irregular shape"

-- | Alias for [fromArraysPartial](#v:fromArraysPartial)
-- | Used in `Show` instance.

mkGrid :: forall a. Partial => Size -> Array (Array a) -> Grid a
mkGrid = fromArraysPartial

-- | Creates a Grid from a Size and a Foldable containing entries for each cell.
-- |
-- | ```
-- | > entries = [(Pos $ Vec 0 0) /\ 'A', (Pos $ Vec 0 1) /\ 'B']
-- | > fromFoldable (Size $ Vec 1 2) entries
-- | (Just (mkGrid (Size (Vec 1 2)) [['A'],['B']]))
-- | ```

fromFoldable :: forall a f. Foldable f => Size -> f (Tuple Pos a) -> Maybe (Grid a)
fromFoldable givenSize xs = do
  guard $ sizeIsValid givenSize
  checkPositions givenSize
  pure $ UnsafeGrid givenSize newMap
  where
  newMap = Map.fromFoldable xs

  checkPositions size' =
    let
      positions' = positionsFromSize size'
    in
      do
        guard (Arr.length positions' == length xs)
        positions'
          # traverse_ (\pos -> void $ Map.lookup pos newMap)

-- TODO: fromArray :: forall a. Size -> Array a -> Maybe (Grid a)
-- TODO: fromArrayConform :: forall a. Int -> Array a -> Grid a
-- TODO: fromArrayAsRow :: forall a. Array a -> Grid a
-- TODO: fromArrayAsColumn :: forall a. Array a -> Grid a
-- TODO: genGrid :: forall a. Gen (Grid a)
-- TODO: genGridSized :: forall a. Size -> Gen (Grid a)

--------------------------------------------------------------------------------
--- Destructors
--------------------------------------------------------------------------------

-- | Turns a Grid into nested Arrays.
-- | 
-- | ```
-- | > toArrays $ fromArraysConform [[1,2], [3,4]]
-- | [[1,2],[3,4]]
-- | ```

toArrays :: forall a. Grid a -> Array (Array a)
toArrays grid@(UnsafeGrid (Size (Vec w h)) _) =
  range' h <#> mkLine
  where
  mkLine y = range' w <#> \x -> mkCell x y
  mkCell x y = unsafePartial $ gridUnsafeLookup "toArrays" (Pos $ Vec x y) grid

-- | Turns a Grid into any Unfoldable containing entries for each cell.
-- |
-- | ```
-- | > grid = fromArraysConform [[1], [2]]
-- | > (toUnfoldable grid) :: Array _    
-- | [(Tuple (Pos (Vec 0 0)) 1),(Tuple (Pos (Vec 0 1)) 2)]
-- | ```

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
toUnfoldable = gridGetMap >>> Map.toUnfoldable

-- | Gets the size (width and height) of a Grid
-- |
-- | ```
-- | > size $ fromArraysConform [[1,2], [3,4]]                     
-- | (Size (Vec 2 2))
-- | ```

size :: forall a. Grid a -> Size
size = gridGetSize

-- | Finds an entry in a Grid.
-- | 
-- | ```
-- | > grid = fromArraysConform [[1,2], [3,4]]
-- | > findEntry (\_ x -> x >= 2) grid
-- | (Just (Tuple (Pos (Vec 0 1)) 3))
-- | ```

findEntry :: forall a. (Pos -> a -> Boolean) -> Grid a -> Maybe (Tuple Pos a)
findEntry f grid = toUnfoldable grid # Arr.find (uncurry f)

-- | ```
-- | > grid = fromArraysConform [['a','b'], ['c','d']]
-- | > getCell (Pos $ Vec 0 0) grid
-- | (Just 'a')
-- | ```

getCell :: forall a. Pos -> Grid a -> Maybe a
getCell pos grid = Map.lookup pos $ gridGetMap grid

-- | ```
-- | > grid = fromArraysConform [['a','b'], ['c','d']]
-- | > getCellModulo (Pos $ Vec 3 4) grid
-- | 'b'
-- | ```

getCellModulo :: forall a. Pos -> Grid a -> a
getCellModulo (Pos pos) grid = unsafePartial
  $ gridUnsafeLookup "modulo" posSafe grid
  where
  (Size gridSize) = gridGetSize grid
  posSafe = Pos $ vmod pos gridSize

-- | Gets the positions of the grid in column first order
-- |
-- | ```
-- | > positions $ fromArraysConform [['a'], ['b']]         
-- | [(Pos (Vec 0 0)),(Pos (Vec 0 1))]
-- | ```

positions :: forall a. Grid a -> Array Pos
positions = gridGetSize >>> positionsFromSize

-- TODO: getSubGrid :: forall a. Pos -> Size -> Grid a -> Maybe (Grid a)
-- TODO: getSubGridModulo :: forall a. Pos -> Size -> Grid a -> Grid a
-- TODO: getSubGridClip :: forall a. Pos -> Size -> Grid a -> Grid a

--------------------------------------------------------------------------------
--- Grid Modifiers
--------------------------------------------------------------------------------

-- | Rotate the Grid clockwise (90 degree).
-- |
-- | ```
-- | > rotateClockwise $ fromArraysConform [[1,2], [3,4]]
-- | (mkGrid (Size (Vec 2 2)) [[3,1],[4,2]]))
-- | ```

rotateClockwise :: forall a. Grid a -> Grid a
rotateClockwise grid@(UnsafeGrid oldSize _) =
  UnsafeGrid newSize newMap
  where
  newSize = NT.over Size Vec.swap oldSize
  newMap =
    positionsFromSize oldSize
      <#> mkEntry
      # Map.fromFoldable

  maxY = (oldSize # unwrap # Vec.getY) - 1

  mkEntry pos@(Pos (Vec x y)) = Tuple
    (Pos $ Vec (maxY - y) x)
    (unsafePartial $ gridUnsafeLookup "rotate" pos grid)

-- TODO: zipApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- TODO: zip :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: zipWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: explodeApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- TODO: explode :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: explodeWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: bind :: forall a b. Grid a -> (a -> Grid b) -> Grid b
-- TODO: bindWithIndex :: forall a b. Grid a -> (Pos -> a -> Grid b) -> Grid b
-- TODO: join :: forall a b. Grid (Grid a) -> Grid a

mirrorX :: forall a. Grid a -> Grid a
mirrorX grid = UnsafeGrid gridSize newMap
  where
  gridSize@(Size (Vec _ height)) = size grid

  newMap = grid
    # gridGetMap
    # mapModifyKey mkKey

  mkKey (Pos pos) =
    (Pos $ V2.modifyY (maxY - _) pos)

  maxY = height - 1

-- TODO: docs
-- TODO: test
mirrorY :: forall a. Grid a -> Grid a
mirrorY grid = UnsafeGrid gridSize newMap
  where
  gridSize@(Size (Vec width _)) = size grid

  newMap = grid
    # gridGetMap
    # mapModifyKey mkEntry

  mkEntry (Pos pos) =
    (Pos $ V2.modifyX (maxX - _) pos)

  maxX = width - 1

-- TODO: docs
-- TODO: test
-- TODO: handle resize
appendX :: forall a. Grid a -> Grid a -> Grid a
appendX gridL gridR = UnsafeGrid newSize newMap
  where
  Size sizeL = size gridL
  Size sizeR = size gridR

  newSize = Size (Vec add min <*> sizeL <*> sizeR)

  mapRight = gridR
    # gridGetMap
    # mapModifyKey mkKey

  mkKey (Pos vec) = Pos $ vec + moveVec

  moveVec = oneX * sizeL

  newMap = gridL
    # gridGetMap
    # Map.union mapRight

-- TODO: docs
-- TODO: test
-- TODO: handle resize

appendY :: forall a. Grid a -> Grid a -> Grid a
appendY gridTop gridBot = UnsafeGrid newSize newMap
  where
  Size sizeTop = size gridTop
  Size sizeBot = size gridBot

  newSize = Size (Vec min add <*> sizeTop <*> sizeBot)

  mapBot = gridBot
    # gridGetMap
    # mapModifyKey mkKey

  mkKey (Pos vec) = Pos $ vec + moveVec

  moveVec = oneY * sizeTop

  newMap = gridTop
    # gridGetMap
    # Map.union mapBot

resize :: forall a. Size -> Grid a -> Maybe (Grid a)
resize unsafeSize (UnsafeGrid (Size oldSize) _)
  | (Size givenSize) <- sizeNormalize unsafeSize
  , and ((>=) <$> givenSize <*> oldSize) =
      Nothing

resize unsafeSize (UnsafeGrid _ gridMap) = Just $ UnsafeGrid newSize newMap
  where
  newSize@(Size newSize') = sizeNormalize unsafeSize
  newMap = gridMap
    # mapModfiyEntries mkEntries

  mkEntries xs = xs
    # Arr.filter filterEntry

  filterEntry (Tuple (Pos pos) _) =
    and ((<) <$> pos <*> newSize')

-- TODO: resizeFit :: forall a. Size -> a -> Grid a -> Grid a
-- TODO: resizeTry :: forall a. Size -> a -> Grid a -> Grid a

--------------------------------------------------------------------------------
--- SubGrid Modifiers
--------------------------------------------------------------------------------

setSubGrid :: forall a. Pos -> Grid a -> Grid a -> Maybe (Grid a)
setSubGrid vec src tgt = src
  # toUnfoldable
  # Arr.foldM (\grid (Tuple k v) -> setCell (vec + k) v grid) tgt

setSubGridClip :: forall a. Pos -> Grid a -> Grid a -> Grid a
setSubGridClip vec src tgt = src
  # (toUnfoldable :: _ -> Array _)
  # foldl (\grid (Tuple k v) -> fromMaybe grid $ setCell (vec + k) v grid) tgt

-- TODO: Test
-- TODO: docs
setSubGridTry :: forall a. Pos -> Grid a -> Grid a -> Grid a
setSubGridTry pos subGrid grid =
  setSubGrid pos subGrid grid # fromMaybe grid

-- TODO: test
-- TODO: docs
setSubGridModulo :: forall a. Pos -> Grid a -> Grid a -> Grid a
setSubGridModulo vec src tgt = src
  # (toUnfoldable :: _ -> Array _)
  # foldl (\grid (Tuple k v) -> setCellModulo (vec + k) v grid) tgt

-- TODO: modifySubGrid :: forall a. Pos -> Size -> (a -> a) -> Maybe (Grid a)
-- TODO: setSubGridModulo :: forall a. Pos -> Grid a -> Grid a
-- TODO: modifySubGridModulo :: forall a. Pos -> Size -> (a -> a) -> Grid a
-- TODO: modifySubGridClip :: forall a. Pos -> Size -> (a -> a) -> Grid a
-- TODO: modifySubGridTry :: Pos -> Size -> (a -> a) -> Grid a

--------------------------------------------------------------------------------
--- Cell Modifiers
--------------------------------------------------------------------------------

-- | ```
-- | > grid = fromArraysConform [[1,2], [3,4]]
-- | > setCell (Pos $ Vec 0 0) 9 grid
-- | (Just (mkGrid (Size (Vec 2 2)) [[9,2],[3,4]]))
-- | ```

setCell :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
setCell pos x (UnsafeGrid oldSize oldMap) | isInSize pos oldSize =
  Just $ UnsafeGrid oldSize newMap
  where
  newMap = Map.insert pos x oldMap
setCell _ _ _ = Nothing

-- | ```
-- | > grid = fromArraysConform [[1,2], [3,4]]
-- | > setCellTry (Pos $ Vec 0 0) 9 grid
-- | (mkGrid (Size (Vec 2 2)) [[9,2],[3,4]])
-- | ```

setCellTry :: forall a. Pos -> a -> Grid a -> Grid a
setCellTry pos x grid = setCell pos x grid # fromMaybe grid

-- TODO: Doc
-- TODO: Test
setCellModulo :: forall a. Pos -> a -> Grid a -> Grid a
setCellModulo (Pos pos) x grid = setCellTry newPos x grid
  where
  (Size gridSize) = size grid
  newPos = Pos (mod <$> pos <*> gridSize)

-- TODO: modifyCell :: forall a. Pos -> (a -> a) -> Grid a -> Maybe (Grid a) 
-- TODO: modifyCellModulo :: forall a. Pos -> (a -> a) -> Grid a -> Grid a 
-- TODO: modifyCellTry :: forall a. Pos -> (a -> a) -> Grid a -> Grid a

--------------------------------------------------------------------------------
--- Pretty Printing
--------------------------------------------------------------------------------

-- | Print a grid without options
-- | 
-- | ```
-- | grid = G.fromArraysConform
-- |   [ [ "bird", "dog" ]
-- |   , [ "cat", "horse" ]
-- |   , [ "monkey", "giraffe" ]
-- |   ]
-- | ```
-- | ---
-- | ```
-- | > printGrid_ grid
-- | bird    dog
-- | cat     horse
-- | monkey  giraffe
-- | ```

printGrid_ :: Grid String -> String
printGrid_ = printGrid defaultPrintOpts

newtype CellFormatter = CellFormatter (Int -> String -> String)

derive instance Newtype CellFormatter _

-- | Printing Options
-- |
-- |  - formatCell formats the string in each cell
-- |  - colSep separator between columns
-- |  - rowSep separator between rows

type PrintOpts =
  { formatCell :: CellFormatter
  , colSep :: String
  , rowSep :: String
  }

-- | Some defaults for `PrintOpts`

defaultPrintOpts :: PrintOpts
defaultPrintOpts =
  { formatCell: padRight ' '
  , colSep: " "
  , rowSep: "\n"
  }

-- | `CellFormatter` that adds a padding to the left
-- |
-- | ```
-- | > (unwrap $ G.padLeft '.') 10 "Hello"
-- | .....Hello
-- | ```

padLeft :: Char -> CellFormatter
padLeft char = CellFormatter \n str -> stringPad char n str <> str

-- | `CellFormatter` that adds a padding to the right
-- |
-- | ```
-- | > (unwrap $ G.padRight '.') 10 "Hello" 
-- | Hello.....
-- | ```

padRight :: Char -> CellFormatter
padRight char = CellFormatter \n str -> str <> stringPad char n str

-- | Print a grid with options
-- | 
-- | ```
-- | grid = G.fromArraysConform
-- |   [ [ "bird", "dog" ]
-- |   , [ "cat", "horse" ]
-- |   , [ "monkey", "giraffe" ]
-- |   ]
-- | ```
-- | ---
-- | ```
-- | > printGrid defaultPrintOpts { formatCell = padLeft '.' }
-- | ...bird ....dog
-- | ....cat ..horse
-- | .monkey giraffe
-- | ```

printGrid :: PrintOpts -> Grid String -> String
printGrid opts grid = grid
  # toArrays
  <#> mkLine
  # Str.joinWith opts.rowSep
  where
  mkLine = map (unwrap opts.formatCell $ maxLength)
    >>> Str.joinWith opts.colSep

  maxLength = grid
    # gridValues
    <#> Str.length
    # Arr.sort
    # Arr.last
    # fromMaybe 0

--------------------------------------------------------------------------------
--- Internal Util
--------------------------------------------------------------------------------

range' :: Int -> Array Int
range' end = unfoldr go zero
  where
  go i | i == end = Nothing
  go i = Just (Tuple i (i + 1))

positionsFromSize :: Size -> Array Pos
positionsFromSize (Size (Vec w h)) = ado
  x <- range' w
  y <- range' h
  in Pos $ Vec x y

isInSize :: Pos -> Size -> Boolean
isInSize (Pos (Vec x y)) (Size (Vec w h)) =
  inRange x w && inRange y h

inRange :: Int -> Int -> Boolean
inRange x w = 0 <= x && x < w

--------------------------------------------------------------------------------
--- Size
--------------------------------------------------------------------------------

sizeNormalize :: Size -> Size
sizeNormalize size' | sizeIsValid size' = size'
sizeNormalize _ = Size $ Vec 0 0

sizeIsValid :: Size -> Boolean
sizeIsValid (Size (Vec 0 0)) = true
sizeIsValid (Size (Vec x y)) | x >= 1 && y >= 1 = true
sizeIsValid _ = false

--------------------------------------------------------------------------------
--- Grid
--------------------------------------------------------------------------------

gridGetSize :: forall a. Grid a -> Size
gridGetSize (UnsafeGrid siz _) = siz

gridGetMap :: forall a. Grid a -> Map Pos a
gridGetMap (UnsafeGrid _ mp) = mp

gridValues :: forall a. Grid a -> Array a
gridValues (UnsafeGrid _ gridMap) = gridMap
  # Map.values
  # List.toUnfoldable

gridUnsafeLookup :: forall a. Partial => String -> Pos -> Grid a -> a
gridUnsafeLookup hint pos grid = getCell pos grid
  # fromMaybe' (\_ -> unsafeCrashWith $ "impossible lookup: " <> hint)

--------------------------------------------------------------------------------
--- String
--------------------------------------------------------------------------------

stringSpaced :: String -> String
stringSpaced x = " " <> x <> " "

stringPad :: Char -> Int -> String -> String
stringPad char n str = buffer
  where
  buffer = Arr.replicate len char # fromCharArray
  len = n - Str.length str

--------------------------------------------------------------------------------
--- Array2d
--------------------------------------------------------------------------------

type Array2d a = Array (Array a)

array2dMaxSize :: forall a. Array (Array a) -> Size
array2dMaxSize xs = sizeNormalize $ newSize
  where
  newSize = Size $ Vec width height
  width = xs <#> Arr.length # Arr.sort # Arr.last # fromMaybe 0
  height = Arr.length xs

array2dLookupPartial :: forall a. Partial => String -> Pos -> Array (Array a) -> a
array2dLookupPartial hint pos xs = case array2dLookup pos xs of
  Just x -> x
  Nothing -> unsafeCrashWith $ "impossible 2d lookup: " <> hint

array2dLookup :: forall a. Pos -> Array (Array a) -> Maybe a
array2dLookup (Pos (Vec x y)) = flip Arr.index y >=> flip Arr.index x

--------------------------------------------------------------------------------
--- Map
--------------------------------------------------------------------------------

mapModfiyEntries :: forall k v. Ord k => (Array (Tuple k v) -> Array (Tuple k v)) -> Map k v -> Map k v
mapModfiyEntries f mp = mp
  # Map.toUnfoldable
  # f
  # Map.fromFoldable

mapModifyEntry :: forall k v. Ord k => (Tuple k v -> Tuple k v) -> Map k v -> Map k v
mapModifyEntry f mp = mp
  # mapModfiyEntries (map f)

mapModifyKey :: forall k v. Ord k => (k -> k) -> Map k v -> Map k v
mapModifyKey f mp = mp
  # mapModifyEntry (lmap f)