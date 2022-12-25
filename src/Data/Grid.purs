module Data.Grid
  ( ErrorFromArrays(..)
  , Grid
  , Pos(..)
  , Size(..)
  , Vec
  , attemptInsert
  , positions
  , empty
  , fill
  , findEntry
  , fromArray
  , fromArrays
  , fromArraysAdjusted
  , fromArraysAdjustedTo
  , insert
  , insertSubgrid
  , insertSubgridCropped
  , isInSize
  , lookup
  , lookupModulo
  , module Exp
  , rotateClockwise
  , size
  , toArrays
  , toUnfoldable
  ) where

import Prelude

import Data.Array as Arr
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
import Linear.Vec2 (Vec2(..))
import Linear.Vec2 (Vec2, vec2) as Exp
import Linear.Vec2 as V2
import Linear.Vec2 as Vec2
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type Vec = Vec2 Int

newtype Size = Size Vec

newtype Pos = Pos Vec

derive newtype instance Ord Pos

data Grid a = UnsafeGrid Size (Map Pos a)

--

derive newtype instance Show Size
derive newtype instance Show Pos

---

class IsVec a where
  toVec :: a -> Vec
  fromVec :: Vec -> a

instance IsVec Size where
  toVec (Size vec) = vec
  fromVec = Size

instance IsVec Pos where
  toVec (Pos vec) = vec
  fromVec = Pos

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive newtype instance Semiring Pos

derive instance Newtype Size _

derive instance Eq Size

derive instance Eq Pos

derive instance (Eq a) => Eq (Grid a)

derive instance Functor Grid

instance FunctorWithIndex Pos Grid where
  mapWithIndex f (UnsafeGrid siz mp) = UnsafeGrid siz $ mapWithIndex f mp

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

-- instance TraversableWithIndex Vec Grid where
--   traverseWithIndex f (UnsafeGrid size mp) = UnsafeGrid size <$> traverseWithIndex f mp

instance Traversable Grid where
  traverse f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> traverse f oldMap
  sequence = sequenceDefault

instance TraversableWithIndex Pos Grid where
  traverseWithIndex f (UnsafeGrid oldSize oldMap) =
    UnsafeGrid oldSize <$> (traverseWithIndex f oldMap)

getSize :: forall a. Grid a -> Size
getSize (UnsafeGrid siz _) = siz

getEntries :: forall a. Grid a -> Map Pos a
getEntries (UnsafeGrid _ mp) = mp

fill :: forall a. Size -> (Pos -> a) -> Grid a
fill givenSize f = UnsafeGrid givenSize newMap
  where
  newMap = positionsFromSize givenSize <#> mkEntry # Map.fromFoldable
  mkEntry k = Tuple k (f k)

empty :: forall a. Grid a
empty = UnsafeGrid (Size zero) Map.empty

insert :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
insert pos x (UnsafeGrid oldSize oldMap) | isInSize pos oldSize =
  Just $ UnsafeGrid oldSize newMap
  where
  newMap = Map.insert pos x oldMap
insert _ _ _ = Nothing

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

fromArray :: forall a. Size -> a -> Array a -> Grid a
fromArray givenSize@(Size (Vec2 w _)) def xs = UnsafeGrid givenSize newMap
  where
  newMap = positionsFromSize givenSize
    <#> mkEntry
    # Map.fromFoldable

  mkEntry pos@(Pos (Vec2 x y)) =
    Tuple pos $
      Arr.index xs (y * w + x) # fromMaybe def

toArrays :: forall a. Grid a -> Array (Array a)
toArrays grid@(UnsafeGrid (Size (Vec2 w h)) _) =
  Arr.range 0 (h - 1) <#> mkLine
  where
  mkLine y = Arr.range 0 (w - 1) <#> \x -> mkCell x y
  mkCell x y = unsafePartial $ unsafeLookup (Pos $ Vec2 x y) grid

rotateClockwise :: forall a. Grid a -> Grid a
rotateClockwise grid@(UnsafeGrid oldSize _) =
  UnsafeGrid newSize newMap
  where
  newSize = NT.over Size Vec2.swap oldSize
  newMap =
    positionsFromSize oldSize
      <#> mkEntry
      # Map.fromFoldable

  maxY = (oldSize # unwrap # V2.getY) - 1

  mkEntry pos@(Pos (Vec2 x y)) = Tuple
    (Pos $ Vec2 (maxY - y) x)
    (unsafePartial $ unsafeLookup pos grid)

lookup :: forall a. Pos -> Grid a -> Maybe a
lookup pos grid = Map.lookup pos $ getEntries grid

lookupModulo :: forall a. Pos -> Grid a -> a
lookupModulo (Pos (Vec2 x y)) grid = unsafePartial $ unsafeLookup posSafe grid
  where
  (Size (Vec2 w h)) = getSize grid
  posSafe = Pos $ Vec2
    (mod x w)
    (mod y h)

-- | Gets the positions of the grid in column first order 
positions :: forall a. Grid a -> Array Pos
positions = getSize >>> positionsFromSize

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
toUnfoldable = getEntries >>> Map.toUnfoldable

size :: forall a. Grid a -> Size
size = getSize

findEntry :: forall a. (Tuple Pos a -> Boolean) -> Grid a -> Maybe (Tuple Pos a)
findEntry f grid = toUnfoldable grid # Arr.find f

attemptInsert :: forall a. Pos -> a -> Grid a -> Grid a
attemptInsert pos x grid = insert pos x grid # fromMaybe grid

insertSubgrid :: forall a. Pos -> Grid a -> Grid a -> Maybe (Grid a)
insertSubgrid vec src tgt = src
  # toUnfoldable
  # Arr.foldM (\grid (Tuple k v) -> insert (vec + k) v grid) tgt

insertSubgridCropped :: forall a. Pos -> Grid a -> Grid a -> Grid a
insertSubgridCropped vec src tgt = src
  # (toUnfoldable :: _ -> Array _)
  # foldl (\grid (Tuple k v) -> fromMaybe grid $ insert (vec + k) v grid) tgt

data ErrorFromArrays = ErrLineWrongLength { guessedSize :: Size, pos :: Pos }

derive instance Generic ErrorFromArrays _

derive instance Eq ErrorFromArrays

instance Show ErrorFromArrays where
  show = genericShow

fromArraysAdjustedTo :: forall a. Size -> a -> Array (Array a) -> Grid a
fromArraysAdjustedTo siz def xs =
  fill siz (\pos -> lookup2d pos xs # fromMaybe def)

--------------------------------------------------------------------------------
--- Util
--------------------------------------------------------------------------------

range' :: Int -> Array Int
range' end = unfoldr go zero
  where
  go i | i == end = Nothing
  go i = Just (Tuple i (i + 1))

positionsFromSize :: Size -> Array Pos
positionsFromSize (Size (Vec2 w h)) = ado
  x <- range' w
  y <- range' h
  in Pos $ Vec2 x y

isInSize :: Pos -> Size -> Boolean
isInSize (Pos (Vec2 x y)) (Size (Vec2 w h)) =
  inRange x w && inRange y h

inRange :: Int -> Int -> Boolean
inRange x w = 0 <= x && x < w

guessSize :: forall a. Array (Array a) -> Size
guessSize xs = Size $ Vec2 x y
  where
  x = Arr.length <<< fromMaybe [] <<< Arr.head $ xs
  y = Arr.length xs

lookup2d :: forall a. Pos -> Array (Array a) -> Maybe a
lookup2d (Pos (Vec2 x y)) = index' y >=> index' x

index' :: forall a. Int -> Array a -> Maybe a
index' = flip Arr.index

unsafeLookup :: forall a. Partial => Pos -> Grid a -> a
unsafeLookup pos grid = lookup pos grid # fromMaybe' (\_ -> unsafeCrashWith "impossible lookup")
