-- | Intro
-- |
-- | - Types
-- |   - [Grid](#t:Grid)
-- |   - [Pos](#t:Pos)
-- |   - [Size](#t:Size)
-- |
-- | - Constructors
-- |   - [empty](#v:empty)
-- |   - [singleton](#v:singleton)
-- |   - [fill](#v:fill)
-- |   - fromArrayAsRow
-- |   - fromArrayAsColumn 
-- |   - [fromFlatArray](#v:fromFlatArray)
-- |   - fromFlatArrayClip
-- |   - fromFlatArrayExtend
-- |   - fromFlatArrayFitTo
-- |   - [ErrorFromArrays](#t:ErrorFromArrays)
-- |   - [fromArrays](#v:fromArrays)
-- |   - [fromArraysPartial](#v:fromArraysPartial)
-- |   - fromArraysClip
-- |   - [fromArraysExtend](#v:fromArraysExtend)
-- |   - [fromArraysFitTo](#v:fromArraysFitTo)
-- |   - fromFoldable
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

module Data.Grid
  ( Grid
  , Pos(..)
  , Size(..)

  , empty
  , singleton
  , fill
  , fromFlatArrayFitTo
  , ErrorFromArrays(..)
  , fromArrays
  , fromArraysPartial
  , fromArraysExtend
  , fromArraysFitTo

  , toArrays
  , toUnfoldable
  , size
  , findEntry
  , getCell
  , getCellModulo
  , positions

  , rotateClockwise

  , setCell
  , trySetCell

  , setSubGrid
  , setSubGridClip

  , module Exp
  ) where

import Prelude

import Data.Array as Arr
import Data.Array.NonEmpty (fromArray, fromFoldable)
import Data.Either (Either(..), fromRight')
import Data.Either as Either
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, intercalate)
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
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vector2 (Vec(..))
import Data.Vector2 (Vec(..)) as Exp
import Data.Vector2 as Vec
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

-- | 2D grid with variable size

data Grid a = UnsafeGrid Size (Map Pos a)

-- | Equality of each cell
-- |
-- | ```
-- | > gridA = fromArraysExtend 0 [[1,2],[3,4]]
-- | > gridB = fromArraysExtend 0 [[1,2],[3,4]]  
-- | > gridA == gridB
-- | true
-- | ```

derive instance (Eq a) => Eq (Grid a)

-- | Maps over each cell
-- |
-- | ```
-- | > map (add 10) $ fromArraysExtend 0 [[1,2],[3,4]]
-- | (fromArraysPartial [[11,12],[13,14]])
-- | ```

derive instance Functor Grid

-- | Maps over each cell with position as index
-- |
-- | ```
-- | > grid = fromArraysExtend " " [["A","B"],["C","D"]]
-- | > mapFn (Pos (Vec x y)) cell = show x <> show y <> cell
-- | > mapWithIndex mapFn grid
-- | (fromArraysPartial [["00A","10B"],["01C","11D"]])
-- | ```

instance FunctorWithIndex Pos Grid where
  mapWithIndex f (UnsafeGrid siz mp) = UnsafeGrid siz $ mapWithIndex f mp

-- | Show instance intended for debugging.
-- |
-- | ```
-- | > logShow $ fromArraysExtend 0 [[1,2],[3,4]]
-- | (fromArraysPartial [[1,2],[3,4]])
-- | ```

instance Show a => Show (Grid a) where
  show grid = fold [ "(fromArraysPartial", " ", show $ toArrays grid, ")" ]

-- | Fold over each cell.
-- |
-- | ```
-- | > grid = fromArraysExtend 0 [[1,2],[3,4]]
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
-- | > grid = fromArraysExtend 0 [[1,2],[3,4]]
-- | > foldFn (Pos (Vec x y)) n acc = x + y + n + acc
-- | > foldrWithIndex foldFn 10 grid
-- | 24
-- | ```

instance FoldableWithIndex Pos Grid where
  foldrWithIndex f x = getEntries >>> foldrWithIndex f x
  foldlWithIndex f x = getEntries >>> foldlWithIndex f x
  foldMapWithIndex = foldMapWithIndexDefaultL

-- | ```
-- | > grid = fromArraysExtend Nothing [[Just 1,Just 2],[Just 3,Just 4]]
-- | > sequence grid
-- | (Just (fromArraysPartial [[1,2],[3,4]]))
-- | ```

instance Traversable Grid where
  traverse f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> traverse f oldMap
  sequence = sequenceDefault

-- | ```
-- | > grid = fromArraysExtend 0 [[1,2],[3,4]]
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
-- | > fill (Size $ Vec 2 2) (\(Pos (Vec x y)) -> show x <> "-" <> show y)
-- | (fromArraysPartial [["0-0","1-0"],["0-1","1-1"]])
-- | ```

fill :: forall a. Size -> (Pos -> a) -> Grid a
fill unsafeSize f = UnsafeGrid givenSize newMap
  where
  givenSize = normalizeSize unsafeSize
  newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  mkEntry k = Tuple k (f k)

fromFlatArrayFitTo :: forall a. Size -> a -> Array a -> Grid a
fromFlatArrayFitTo unsafeSize def xs =
  UnsafeGrid givenSize newMap
  where
  givenSize@(Size (Vec w _)) = normalizeSize unsafeSize
  newMap = positionsFromSize givenSize
    <#> mkEntry
    # Map.fromFoldable

  mkEntry pos@(Pos (Vec x y)) =
    Tuple pos $
      Arr.index xs (y * w + x) # fromMaybe def

-- | Error that can happen inside the `fromArrays` function
data ErrorFromArrays = ErrLineWrongLength { deducedSize :: Size, pos :: Pos }

derive instance Generic ErrorFromArrays _

derive instance Eq ErrorFromArrays

instance Show ErrorFromArrays where
  show = genericShow

printErrorFromArrays :: ErrorFromArrays -> String
printErrorFromArrays (ErrLineWrongLength { deducedSize: Size size, pos: Pos pos }) =
  intercalate "\n"
    [ "Wrong line length!"
    , fold
        [ "Based on the length of the first line it a grid a size of"
        , " " <> printVec size <> " "
        , "was deduced."
        ]
    , fold
        [ "However, position"
        , " " <> printVec pos <> " "
        , "was not found in the input."
        ]
    ]

--"Based on the length of the first line it a grid size of " <> printVec size <> """ 

printVec :: Vec Int -> String
printVec (Vec x y) = fold [ "(", show x, "|", show y, ")" ]

fromArrays :: forall a. Array (Array a) -> Either ErrorFromArrays (Grid a)
fromArrays xs = ado
  newMap <- positionsFromSize newSize # traverse mkEntry <#> Map.fromFoldable
  in
    UnsafeGrid newSize newMap
  where
  mkEntry pos = ado
    value <- lookup2d pos xs # Either.note (ErrLineWrongLength { deducedSize: newSize, pos })
    in Tuple pos value

  newSize = guessSize xs

fromArraysPartial :: forall a. Partial => Array (Array a) -> Grid a
fromArraysPartial xs = case fromArrays xs of
  Left err -> unsafeCrashWith $ printErrorFromArrays err
  Right ok -> ok

fromArraysExtend :: forall a. a -> Array (Array a) -> Grid a
fromArraysExtend def xs = UnsafeGrid newSize newMap
  where
  mkEntry pos = Tuple pos (lookup2d pos xs # fromMaybe def)
  newSize = guessSize xs
  newMap = positionsFromSize newSize <#> mkEntry # Map.fromFoldable

fromArraysFitTo :: forall a. Size -> a -> Array (Array a) -> Grid a
fromArraysFitTo siz def xs =
  fill siz (\pos -> lookup2d pos xs # fromMaybe def)

--------------------------------------------------------------------------------
--- Destructors
--------------------------------------------------------------------------------

toArrays :: forall a. Grid a -> Array (Array a)
toArrays grid@(UnsafeGrid (Size (Vec w h)) _) =
  Arr.range 0 (h - 1) <#> mkLine
  where
  mkLine y = Arr.range 0 (w - 1) <#> \x -> mkCell x y
  mkCell x y = unsafePartial $ unsafeLookup (Pos $ Vec x y) grid

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
toUnfoldable = getEntries >>> Map.toUnfoldable

size :: forall a. Grid a -> Size
size = getSize

findEntry :: forall a. (Tuple Pos a -> Boolean) -> Grid a -> Maybe (Tuple Pos a)
findEntry f grid = toUnfoldable grid # Arr.find f

getCell :: forall a. Pos -> Grid a -> Maybe a
getCell pos grid = Map.lookup pos $ getEntries grid

getCellModulo :: forall a. Pos -> Grid a -> a
getCellModulo (Pos (Vec x y)) grid = unsafePartial $ unsafeLookup posSafe grid
  where
  (Size (Vec w h)) = getSize grid
  posSafe = Pos $ Vec
    (mod x w)
    (mod y h)

-- | Gets the positions of the grid in column first order 
positions :: forall a. Grid a -> Array Pos
positions = getSize >>> positionsFromSize

--------------------------------------------------------------------------------
--- Grid Modifiers
--------------------------------------------------------------------------------

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
    (unsafePartial $ unsafeLookup pos grid)

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

--------------------------------------------------------------------------------
--- Cell Modifiers
--------------------------------------------------------------------------------

setCell :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
setCell pos x (UnsafeGrid oldSize oldMap) | isInSize pos oldSize =
  Just $ UnsafeGrid oldSize newMap
  where
  newMap = Map.insert pos x oldMap
setCell _ _ _ = Nothing

trySetCell :: forall a. Pos -> a -> Grid a -> Grid a
trySetCell pos x grid = setCell pos x grid # fromMaybe grid

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

guessSize :: forall a. Array (Array a) -> Size
guessSize xs = Size $ Vec x y
  where
  x = Arr.length <<< fromMaybe [] <<< Arr.head $ xs
  y = Arr.length xs

lookup2d :: forall a. Pos -> Array (Array a) -> Maybe a
lookup2d (Pos (Vec x y)) = index' y >=> index' x

index' :: forall a. Int -> Array a -> Maybe a
index' = flip Arr.index

unsafeLookup :: forall a. Partial => Pos -> Grid a -> a
unsafeLookup pos grid = getCell pos grid # fromMaybe' (\_ -> unsafeCrashWith "impossible lookup")

normalizeSize :: Size -> Size
normalizeSize (Size vec) = Size $ max 0 <$> vec

getSize :: forall a. Grid a -> Size
getSize (UnsafeGrid siz _) = siz

getEntries :: forall a. Grid a -> Map Pos a
getEntries (UnsafeGrid _ mp) = mp

--------------------------------------------------------------------------------
--- Planned API
--------------------------------------------------------------------------------

-- setCell :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
-- getCell :: forall a. -> Pos -> Grid a -> Maybe a
-- modifyCell :: forall a. Pos -> (a -> a) -> Grid a -> Maybe (Grid a) 

-- setCellModulo :: forall a. Pos -> a -> Grid a -> Grid a
-- getCellModulo :: forall a. -> Pos -> Grid a -> a
-- modifyCellModulo :: forall a. Pos -> (a -> a) -> Grid a -> Grid a 

-- trySetCell :: forall a. Pos -> a -> Grid a -> Grid a
-- tryModifyCell :: forall a. Pos -> (a -> a) -> Grid a -> Grid a

-- setSubGrid :: forall a. Pos -> Grid a -> Maybe (Grid a)
-- getSubGrid :: forall a. Pos -> Size -> Grid a -> Maybe (Grid a)
-- modifySubGrid :: forall a. Pos -> Size -> (a -> a) -> Maybe (Grid a)

-- setSubGridModulo :: forall a. Pos -> Grid a -> Grid a
-- getSubGridModulo :: forall a. Pos -> Size -> Grid a -> Grid a
-- modifySubGridModulo :: forall a. Pos -> Size -> (a -> a) -> Grid a

-- setSubGridClip :: forall a. Pos -> Grid a -> Grid a
-- getSubGridClip :: forall a. Pos -> Size -> Grid a -> Grid a
-- modifySubGridClip :: Pos -> Size -> (a -> a) -> Grid a

-- trySetSubGrid :: forall a. Pos -> Grid a -> Grid a
-- tryModifySubGrid :: Pos -> Size -> (a -> a) -> Grid a

-- zipApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- zip :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- zipWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c

-- explodeApply :: forall a b. Grid (a -> b) -> Grid a -> Grid b
-- explode :: forall a b. (a -> b -> c) -> Grid a -> Grid b -> Grid c
-- explodeWithIndex :: forall a b. (Pos -> a -> b -> c) -> Grid a -> Grid b -> Grid c

-- bind :: Grid a -> (a -> Grid b) -> Grid b
-- bindWithIndex :: Grid a -> (Pos -> a -> Grid b) -> Grid b
-- join :: Grid (Grid a) -> Grid a

-- singleton :: forall a. a -> Grid a

-- arrayToRowVector :: Array a -> Grid a
-- arrayToColumnVector :: Array a -> Grid a

-- fromFoldable :: Size -> f (Pos /\ a) -> Maybe (Grid a)

-- mirrorY :: Grid a -> Grid a
-- mirrorX :: Grid a -> Grid a

-- genGrid :: Gen a -> Gen (Grid a)
-- genGridSized :: Size -> Gen a -> Gen (Grid a)

-- appendY :: Grid a -> Grid a -> Grid a   
-- appendX :: Grid a -> Grid a -> Grid a

-- resize :: Size -> Grid a -> Maybe (Grid a)
-- resizeFit :: Size -> a -> Grid a -> Grid a

---

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