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
-- |   - [fromArrays](#v:fromArrays)
-- |   - [fromArraysConform](#v:fromArraysConform)
-- |   - [fromArraysPartial](#v:fromArraysPartial)
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
-- |   - mirrorY
-- |   - mirrorX
-- |   - appendY
-- |   - appendX
-- |   - resize
-- |   - resizeFit
-- |
-- | - SubGrid Modifiers
-- |   - [setSubGrid](#v:setSubGrid)
-- |   - [setSubGridClip](#v:setSubGridClip)
-- |   - modifySubGrid
-- |   - setSubGridModulo
-- |   - modifySubGridModulo
-- |   - modifySubGridClip
-- |   - trySetSubGrid
-- |   - tryModifySubGrid
-- |
-- | - Cell Modifiers
-- |   - [setCell](#v:setCell) 
-- |   - [trySetCell](#v:trySetCell)
-- |   - modifyCell
-- |   - setCellModulo
-- |   - modifyCellModulo
-- |   - tryModifyCell

--------------------------------------------------------------------------------
--- Exports
--------------------------------------------------------------------------------

module Data.Grid
  ( ErrorFromArrays(..)
  , Grid
  , Pos(..)
  , Size(..)

  , empty
  , singleton
  , fill
  , fromArrays
  , fromArraysConform
  , fromArraysPartial
  , fromFoldable

  , toArrays
  , toUnfoldable
  , size
  , findEntry
  , getCell
  , getCellModulo
  , positions

  , rotateClockwise

  , setSubGrid
  , setSubGridClip
  , setCell
  , trySetCell

  , module Exp
  ) where

--------------------------------------------------------------------------------
--- Imports
--------------------------------------------------------------------------------

import Prelude

import Control.Alternative (guard)
import Data.Array as Arr
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, traverse_)
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
import Data.Vector2 (Vec(..))
import Data.Vector2 (Vec(..)) as Exp
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
-- | (fromArraysPartial [[11,12],[13,14]])
-- | ```

derive instance Functor Grid

-- | Maps over each cell with position as index
-- |
-- | ```
-- | > grid = fromArraysConform [["A","B"],["C","D"]]
-- | > mapFn (Pos (Vec x y)) cell = show x <> show y <> cell
-- | > mapWithIndex mapFn grid
-- | (fromArraysPartial [["00A","10B"],["01C","11D"]])
-- | ```

instance FunctorWithIndex Pos Grid where
  mapWithIndex f (UnsafeGrid siz mp) = UnsafeGrid siz $ mapWithIndex f mp

-- | Show instance intended for debugging.
-- |
-- | ```
-- | > logShow $ fromArraysConform [[1,2],[3,4]]
-- | (fromArraysPartial [[1,2],[3,4]])
-- | ```

instance Show a => Show (Grid a) where
  show grid = fold [ "(fromArraysPartial", " ", show $ toArrays grid, ")" ]

-- | Fold over each cell.
-- |
-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > foldr add 100 grid
-- | 110
-- | ```

instance Foldable Grid where
  foldr f x = getEntries >>> foldr f x
  foldl f x = getEntries >>> foldl f x
  foldMap f = getEntries >>> foldMap f

-- | Fold over each cell with positions as index.
-- |
-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > foldFn (Pos (Vec x y)) n acc = x + y + n + acc
-- | > foldrWithIndex foldFn 10 grid
-- | 24
-- | ```

instance FoldableWithIndex Pos Grid where
  foldrWithIndex f x = getEntries >>> foldrWithIndex f x
  foldlWithIndex f x = getEntries >>> foldlWithIndex f x
  foldMapWithIndex = foldMapWithIndexDefaultL

-- | ```
-- | > grid = fromArraysConform [[Just 1,Just 2],[Just 3,Just 4]]
-- | > sequence grid
-- | (Just (fromArraysPartial [[1,2],[3,4]]))
-- | ```

instance Traversable Grid where
  traverse f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> traverse f oldMap
  sequence = sequenceDefault

-- | ```
-- | > grid = fromArraysConform [[1,2],[3,4]]
-- | > fn (Pos (Vec x _)) _ = Just x 
-- | > traverseWithIndex fn grid
-- | (Just (fromArraysPartial [[0,1],[0,1]]))
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
-- | (fromArraysPartial [['A']])
-- | ```

singleton :: forall a. a -> Grid a
singleton x = UnsafeGrid (Size $ Vec 1 1) (Map.singleton (Pos $ Vec 0 0) x)

-- | Fills a grid with a function based on the position
-- |
-- | ```
-- | > fn (Pos (Vec x y)) = show x <> "-" <> show y
-- | > fill (Size $ Vec 2 2) fn
-- | (fromArraysPartial [["0-0","1-0"],["0-1","1-1"]])
-- | ```

fill :: forall a. Size -> (Pos -> a) -> Grid a
fill unsafeSize f = UnsafeGrid givenSize newMap
  where
  givenSize = normalizeSize unsafeSize
  newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  mkEntry k = Tuple k (f k)

-- | Error that can happen inside the `fromArrays` function
data ErrorFromArrays
  = ErrLineTooShort { width :: Int, line :: Int }
  | ErrLineTooLong { width :: Int, line :: Int }

derive instance Generic ErrorFromArrays _

derive instance Eq ErrorFromArrays

instance Show ErrorFromArrays where
  show = genericShow

printErrorFromArrays :: ErrorFromArrays -> String
printErrorFromArrays = case _ of
  ErrLineTooShort { width, line } ->
    fold
      [ "Line", spaced $ show line, "is too short. Should have length", spaced $ show width ]
  ErrLineTooLong { width, line } ->
    fold
      [ "Line", spaced $ show line, "is too long. Should have length", spaced $ show width ]

-- | Creates a Grid of a given size from a nested Array.
-- | Array structure must exactly match the size.
-- |
-- | ```
-- | > fromArrays (Size $ Vec 2 2) [[1,2],[3,4]]
-- | (Just (fromArraysPartial [[1,2],[3,4]]))
-- | ```

fromArrays :: forall a. Size -> Array (Array a) -> Maybe (Grid a)
fromArrays unsafeSize xs = ado
  checkLinesM
  newMap <- newMapM
  in UnsafeGrid newSize newMap
  where
  newSize@(Size (Vec width height)) = normalizeSize unsafeSize

  checkLinesM =
    guard (array2dMaxSize xs == newSize)

  newMapM = positionsFromSize newSize
    # traverse mkEntryM
    <#> Map.fromFoldable

  mkEntryM pos@(Pos (Vec _ y)) = ado
    value <- lookup2d pos xs
    in Tuple pos value

-- | Creates a Grid from a nested Array.
-- | The height is determined by the length of the outer Array.
-- | The width by the shortest length of the inner Arrays.
-- | With this rules there can exist a Grid for every input.
-- |
-- | ```
-- | > fromArraysConform [[1,2],[3,4]]
-- | (Just (fromArraysPartial [[1,2],[3,4]]))
-- | ```

fromArraysConform :: forall a. Array (Array a) -> Grid a
fromArraysConform xs = UnsafeGrid newSize newMap
  where
  width = xs <#> Arr.length # Arr.sort # Arr.head # fromMaybe 0
  height = Arr.length xs
  newSize = normalizeSize $ Size $ Vec width height

  newMap = positionsFromSize newSize
    <#> mkEntry
    # Map.fromFoldable

  mkEntry pos@(Pos (Vec _ y)) =
    let
      value = unsafePartial $ unsafeLookup2d "fromArraysConform" pos xs
    in
      Tuple pos value

-- | Creates a Grid from a nested Array.
-- | If the inner Arrays differ in length an exception is thrown.
-- | Thus the Partial constraint.
-- |
-- | ```
-- | > fromArraysPartial [[1,2],[3,4]]
-- | (fromArraysPartial [[1,2],[3,4]])
-- | ```

fromArraysPartial :: forall a. Partial => Array (Array a) -> Grid a
fromArraysPartial xs = case fromArrays newSize xs of
  Just x -> x
  Nothing -> unsafeCrashWith "Arrays have irregular shape"
  where
  newSize = array2dMaxSize xs

-- | Creates a Grid from a Size and a Foldable containing entries for each cell.
-- |
-- | ```
-- | > entries = [(Pos $ Vec 0 0) /\ 'A', (Pos $ Vec 0 1) /\ 'B']
-- | > fromFoldable (Size $ Vec 1 1) entries
-- | (Just (fromArraysPartial [['A'],['B']]))
-- | ```

fromFoldable :: forall a f. Foldable f => Size -> f (Tuple Pos a) -> Maybe (Grid a)
fromFoldable unsafeSize xs = ado
  checkPositions
  in UnsafeGrid newSize newMap
  where
  newSize = normalizeSize unsafeSize
  newMap = Map.fromFoldable xs
  checkPositions = positionsFromSize newSize
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
  mkCell x y = unsafePartial $ unsafeLookup "toArrays" (Pos $ Vec x y) grid

-- | Turns a Grid into any Unfoldable containing entries for each cell.
-- |
-- | ```
-- | > grid = fromArraysConform [[1], [2]]
-- | > (toUnfoldable grid) :: Array _    
-- | [(Tuple (Pos (Vec 0 0)) 1),(Tuple (Pos (Vec 0 1)) 2)]
-- | ```

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
toUnfoldable = getEntries >>> Map.toUnfoldable

-- | Gets the size (width and height) of a Grid
-- |
-- | ```
-- | > size $ fromArraysConform [[1,2], [3,4]]                     
-- | (Size (Vec 2 2))
-- | ```

size :: forall a. Grid a -> Size
size = getSize

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
getCell pos grid = Map.lookup pos $ getEntries grid

-- | ```
-- | > grid = fromArraysConform [['a','b'], ['c','d']]
-- | > getCellModulo (Pos $ Vec 3 4) grid
-- | 'b'
-- | ```

getCellModulo :: forall a. Pos -> Grid a -> a
getCellModulo (Pos (Vec x y)) grid = unsafePartial
  $ unsafeLookup "modulo" posSafe grid
  where
  (Size (Vec w h)) = getSize grid
  posSafe = Pos $ Vec
    (mod x w)
    (mod y h)

-- | Gets the positions of the grid in column first order
-- |
-- | ```
-- | > positions $ fromArraysConform [['a'], ['b']]         
-- | [(Pos (Vec 0 0)),(Pos (Vec 0 1))]
-- | ```

positions :: forall a. Grid a -> Array Pos
positions = getSize >>> positionsFromSize

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
-- | (fromArraysPartial [[3,1],[4,2]]))
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
    (unsafePartial $ unsafeLookup "rotate" pos grid)

-- TODO: zipApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- TODO: zip :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: zipWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: explodeApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- TODO: explode :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: explodeWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c
-- TODO: bind :: forall a b. Grid a -> (a -> Grid b) -> Grid b
-- TODO: bindWithIndex :: forall a b. Grid a -> (Pos -> a -> Grid b) -> Grid b
-- TODO: join :: forall a b. Grid (Grid a) -> Grid a
-- TODO: mirrorY :: forall a. Grid a -> Grid a
-- TODO: mirrorX :: forall a. Grid a -> Grid a
-- TODO: appendY :: forall a. Grid a -> Grid a -> Grid a   
-- TODO: appendX :: forall a. Grid a -> Grid a -> Grid a
-- TODO: resize :: forall a. Size -> Grid a -> Maybe (Grid a)
-- TODO: resizeFit :: forall a. Size -> a -> Grid a -> Grid a

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

-- TODO: modifySubGrid :: forall a. Pos -> Size -> (a -> a) -> Maybe (Grid a)
-- TODO: setSubGridModulo :: forall a. Pos -> Grid a -> Grid a
-- TODO: modifySubGridModulo :: forall a. Pos -> Size -> (a -> a) -> Grid a
-- TODO: modifySubGridClip :: forall a. Pos -> Size -> (a -> a) -> Grid a
-- TODO: trySetSubGrid :: forall a. Pos -> Grid a -> Grid a
-- TODO: tryModifySubGrid :: Pos -> Size -> (a -> a) -> Grid a

--------------------------------------------------------------------------------
--- Cell Modifiers
--------------------------------------------------------------------------------

-- | ```
-- | > grid = fromArraysConform [[1,2], [3,4]]
-- | > setCell (Pos $ Vec 0 0) 9 grid
-- | (Just (fromArraysPartial [[9,2],[3,4]]))
-- | ```

setCell :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
setCell pos x (UnsafeGrid oldSize oldMap) | isInSize pos oldSize =
  Just $ UnsafeGrid oldSize newMap
  where
  newMap = Map.insert pos x oldMap
setCell _ _ _ = Nothing

-- | ```
-- | > grid = fromArraysConform [[1,2], [3,4]]
-- | > trySetCell (Pos $ Vec 0 0) 9 grid
-- | (fromArraysPartial [[9,2],[3,4]])
-- | ```

trySetCell :: forall a. Pos -> a -> Grid a -> Grid a
trySetCell pos x grid = setCell pos x grid # fromMaybe grid

-- TODO: modifyCell :: forall a. Pos -> (a -> a) -> Grid a -> Maybe (Grid a) 
-- TODO: setCellModulo :: forall a. Pos -> a -> Grid a -> Grid a
-- TODO: modifyCellModulo :: forall a. Pos -> (a -> a) -> Grid a -> Grid a 
-- TODO: tryModifyCell :: forall a. Pos -> (a -> a) -> Grid a -> Grid a

--------------------------------------------------------------------------------
--- Util
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

lookup2d :: forall a. Pos -> Array (Array a) -> Maybe a
lookup2d (Pos (Vec x y)) = index' y >=> index' x

unsafeLookup2d :: forall a. Partial => String -> Pos -> Array (Array a) -> a
unsafeLookup2d hint pos xs = case lookup2d pos xs of
  Just x -> x
  Nothing -> unsafeCrashWith $ "impossible 2d lookup: " <> hint

index' :: forall a. Int -> Array a -> Maybe a
index' = flip Arr.index

unsafeLookup :: forall a. Partial => String -> Pos -> Grid a -> a
unsafeLookup hint pos grid = getCell pos grid
  # fromMaybe' (\_ -> unsafeCrashWith $ "impossible lookup: " <> hint)

normalizeSize :: Size -> Size
normalizeSize (Size (Vec x _)) | x <= 0 = Size $ Vec 0 0
normalizeSize (Size (Vec _ y)) | y <= 0 = Size $ Vec 0 0
normalizeSize s = s

getSize :: forall a. Grid a -> Size
getSize (UnsafeGrid siz _) = siz

getEntries :: forall a. Grid a -> Map Pos a
getEntries (UnsafeGrid _ mp) = mp

values :: forall a. Grid a -> Array a
values (UnsafeGrid _ gridMap) = gridMap
  # Map.values
  # List.toUnfoldable

printStringGrid :: Grid String -> String
printStringGrid grid = grid
  # toArrays
  <#> mkLine
  # Str.joinWith "\n"
  where
  mkLine = map (padRight (maxLength + 1) ' ') >>> Str.joinWith ""
  maxLength = grid
    # values
    <#> Str.length
    # Arr.sort
    # Arr.head
    # fromMaybe 0

padRight :: Int -> Char -> String -> String
padRight n char str = str <> buffer
  where
  buffer = Arr.replicate len char # fromCharArray
  len = n - Str.length str

spaced :: String -> String
spaced x = " " <> x <> " "

array2dMaxSize :: forall a. Array (Array a) -> Size
array2dMaxSize xs = normalizeSize $ newSize
  where
  newSize = Size $ Vec width height
  width = xs <#> Arr.length # Arr.sort # Arr.last # fromMaybe 0
  height = Arr.length xs
