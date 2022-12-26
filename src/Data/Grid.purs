-- | Intro
-- |
-- | - Types
-- |   - [Grid](#t:Grid)
-- |   - [Pos](#t:Pos)
-- |   - [Size](#t:Size)
-- |
-- | - Errors
-- |   - [ErrorFromArrays](#t:ErrorFromArrays)
-- |
-- | - Constructors
-- |   - [fill](#v:fill)
-- |   - [empty](#v:empty)
-- |   - [fromArrays](#v:fromArrays)
-- |   - [fromArraysAdjusted](#v:fromArraysAdjusted)
-- |   - [fromFlatArray](#v:fromFlatArray)
-- |   - [fromArraysAdjustedTo](#v:fromArraysAdjustedTo)
-- |   - singleton
-- |   - arrayToRowVector
-- |   - arrayToColumnVector
-- |   - fromFoldable
-- |   - genGrid
-- |   - genGridOfSize
-- |
-- | - Destructors
-- |   - [toArrays](#v:toArrays)
-- |   - [toUnfoldable](#v:toUnfoldable)
-- |   - [size](#v:size)
-- |   - [findEntry](#v:findEntry)
-- |   - [getCell](#v:getCell)
-- |   - [getCellModulo](#v:getCellModulo)
-- |   - [positions](#v:positions)
-- |   - getGrid
-- |   - getGridModulo
-- |   - getGridCropped
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
-- |   - resizeAdjusted
-- |
-- | - SubGrid Modifiers
-- |   - [setGrid](#v:setGrid)
-- |   - [setGridCropped](#v:setGridCropped)
-- |   - modifyGrid
-- |   - setGridModulo
-- |   - modifyGridModulo
-- |   - modifyGridCropped
-- |   - trySetGrid
-- |   - tryModifyGrid
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

  , ErrorFromArrays(..)

  , fill
  , empty
  , fromArrays
  , fromArraysAdjusted
  , fromFlatArray
  , fromArraysAdjustedTo

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

  , setGrid
  , setGridCropped

  , module Exp
  ) where

import Prelude

import Data.Array as Arr
import Data.Array.NonEmpty (fromFoldable)
import Data.Either (Either)
import Data.Either as Either
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as NT
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vector2 (Vec(..))
import Data.Vector2 as Vec
import Data.Vector2 (Vec(..)) as Exp
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

newtype Size = Size (Vec Int)

newtype Pos = Pos (Vec Int)

data Grid a = UnsafeGrid Size (Map Pos a)

derive newtype instance Semiring Pos

derive instance Newtype Size _

derive instance Eq Size

derive instance Eq Pos

derive newtype instance Ord Pos

derive instance (Eq a) => Eq (Grid a)

derive instance Functor Grid

instance FunctorWithIndex Pos Grid where
  mapWithIndex f (UnsafeGrid siz mp) = UnsafeGrid siz $ mapWithIndex f mp

derive newtype instance Show Size

derive newtype instance Show Pos

instance Show a => Show (Grid a) where
  show grid = show
    { type: "Grid"
    , size: getSize grid
    , entries: Map.toUnfoldable $ getEntries grid :: Array _
    }

instance Foldable Grid where
  foldr f x = getEntries >>> foldr f x
  foldl f x = getEntries >>> foldl f x
  foldMap f = getEntries >>> foldMap f

instance FoldableWithIndex Pos Grid where
  foldrWithIndex f x = getEntries >>> foldrWithIndex f x
  foldlWithIndex f x = getEntries >>> foldlWithIndex f x
  foldMapWithIndex = foldMapWithIndexDefaultL

instance Traversable Grid where
  traverse f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> traverse f oldMap
  sequence = sequenceDefault

instance TraversableWithIndex Pos Grid where
  traverseWithIndex f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> (traverseWithIndex f oldMap)


--------------------------------------------------------------------------------
--- Errors
--------------------------------------------------------------------------------

data ErrorFromArrays = ErrLineWrongLength { guessedSize :: Size, pos :: Pos }

derive instance Generic ErrorFromArrays _

derive instance Eq ErrorFromArrays

instance Show ErrorFromArrays where
  show = genericShow



--------------------------------------------------------------------------------
--- Constructors
--------------------------------------------------------------------------------

-- | Fills a grid with a function based on the position

fill :: forall a. Size -> (Pos -> a) -> Grid a
fill unsafeSize f = UnsafeGrid givenSize newMap
  where
  givenSize = normalizeSize unsafeSize
  newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  mkEntry k = Tuple k (f k)

empty :: forall a. Grid a
empty = UnsafeGrid (Size zero) Map.empty

fromArrays :: forall a. Array (Array a) -> Either ErrorFromArrays (Grid a)
fromArrays xs = ado
  newMap <- positionsFromSize newSize # traverse mkEntry <#> Map.fromFoldable
  in
    UnsafeGrid newSize newMap
  where
  mkEntry pos = ado
    value <- lookup2d pos xs # Either.note (ErrLineWrongLength { guessedSize: newSize, pos })
    in Tuple pos value

  newSize = guessSize xs

fromArraysAdjusted :: forall a. a -> Array (Array a) -> Grid a
fromArraysAdjusted def xs = UnsafeGrid newSize newMap
  where
  mkEntry pos = Tuple pos (lookup2d pos xs # fromMaybe def)
  newSize = guessSize xs
  newMap = positionsFromSize newSize <#> mkEntry # Map.fromFoldable

fromFlatArray :: forall a. Size -> a -> Array a -> Grid a
fromFlatArray unsafeSize def xs =
  UnsafeGrid givenSize newMap
  where
  givenSize@(Size (Vec w _)) = normalizeSize unsafeSize
  newMap = positionsFromSize givenSize
    <#> mkEntry
    # Map.fromFoldable

  mkEntry pos@(Pos (Vec x y)) =
    Tuple pos $
      Arr.index xs (y * w + x) # fromMaybe def

fromArraysAdjustedTo :: forall a. Size -> a -> Array (Array a) -> Grid a
fromArraysAdjustedTo siz def xs =
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

setGrid :: forall a. Pos -> Grid a -> Grid a -> Maybe (Grid a)
setGrid vec src tgt = src
  # toUnfoldable
  # Arr.foldM (\grid (Tuple k v) -> setCell (vec + k) v grid) tgt

setGridCropped :: forall a. Pos -> Grid a -> Grid a -> Grid a
setGridCropped vec src tgt = src
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

-- setGrid :: forall a. Pos -> Grid a -> Maybe (Grid a)
-- getGrid :: forall a. Pos -> Size -> Grid a -> Maybe (Grid a)
-- modifyGrid :: forall a. Pos -> Size -> (a -> a) -> Maybe (Grid a)

-- setGridModulo :: forall a. Pos -> Grid a -> Grid a
-- getGridModulo :: forall a. Pos -> Size -> Grid a -> Grid a
-- modifyGridModulo :: forall a. Pos -> Size -> (a -> a) -> Grid a

-- setGridCropped :: forall a. Pos -> Grid a -> Grid a
-- getGridCropped :: forall a. Pos -> Size -> Grid a -> Grid a
-- modifyGridCropped :: Pos -> Size -> (a -> a) -> Grid a

-- trySetGrid :: forall a. Pos -> Grid a -> Grid a
-- tryModifyGrid :: Pos -> Size -> (a -> a) -> Grid a

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
-- genGridOfSize :: Size -> Gen a -> Gen (Grid a)

-- appendY :: Grid a -> Grid a -> Grid a   
-- appendX :: Grid a -> Grid a -> Grid a

-- resize :: Size -> Grid a -> Maybe (Grid a)
-- resizeAdjusted :: Size -> a -> Grid a -> Grid a