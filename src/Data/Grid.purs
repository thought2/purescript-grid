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
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe')
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Linear.Vec2 (Vec2, getX, getY, vec2)
import Linear.Vec2 (Vec2, vec2) as Exp
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
  traverse f = unsafeOverEntriesM $ traverse f
  sequence = sequenceDefault

instance TraversableWithIndex Pos Grid where
  traverseWithIndex f = unsafeOverEntriesM $ traverseWithIndex f

--------------------------------------------------------------------------------
--- Unsafe Impl
--------------------------------------------------------------------------------

unsafeOverEntries :: forall a b. (Map Pos a -> Map Pos b) -> Grid a -> Grid b
unsafeOverEntries f (UnsafeGrid siz mp) = UnsafeGrid siz (f mp)

unsafeOverEntriesM :: forall a b m. Functor m => (Map Pos a -> m (Map Pos b)) -> Grid a -> m (Grid b)
unsafeOverEntriesM f (UnsafeGrid siz mp) = UnsafeGrid siz <$> (f mp)

getSize :: forall a. Grid a -> Size
getSize (UnsafeGrid siz _) = siz

getEntries :: forall a. Grid a -> Map Pos a
getEntries (UnsafeGrid _ mp) = mp

unsafeFill :: forall a. Size -> (Pos -> a) -> Grid a
unsafeFill siz f = UnsafeGrid siz $
  positionsFromSize siz <#> mkEntry # Map.fromFoldable
  where
  mkEntry k = Tuple k (f k)

unsafeEmpty :: forall a. Grid a
unsafeEmpty = UnsafeGrid (Size zero) Map.empty

unsafeInsert :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
unsafeInsert pos x grid | isInSize pos (getSize grid) =
  Just $ unsafeOverEntries (Map.insert pos x) grid
unsafeInsert _ _ _ = Nothing

--------------------------------------------------------------------------------
--- Safe Impl
--------------------------------------------------------------------------------

fill :: forall a. Size -> (Pos -> a) -> Grid a
fill = unsafeFill

empty :: forall a. Grid a
empty = unsafeEmpty

lookup :: forall a. Pos -> Grid a -> Maybe a
lookup pos grid = Map.lookup pos $ getEntries grid

lookupModulo :: forall a. Pos -> Grid a -> a
lookupModulo (Pos pos) grid = lookup posSafe grid
  # fromMaybe' (\_ -> unsafeCrashWith "Modulo lookup")
  where
  (Size siz) = getSize grid
  posSafe = Pos $ vec2
    (mod (getX pos) (getX siz))
    (mod (getY pos) (getY siz))

-- | Gets the positions of the grid in column first order 
positions :: forall a. Grid a -> Array Pos
positions = getSize >>> positionsFromSize

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Tuple Pos a)
toUnfoldable = getEntries >>> Map.toUnfoldable

size :: forall a. Grid a -> Size
size = getSize

findEntry :: forall a. (Tuple Pos a -> Boolean) -> Grid a -> Maybe (Tuple Pos a)
findEntry f grid = toUnfoldable grid # Arr.find f

insert :: forall a. Pos -> a -> Grid a -> Maybe (Grid a)
insert = unsafeInsert

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

fromArrays :: forall a. Array (Array a) -> Either ErrorFromArrays (Grid a)
fromArrays xs = do
  mp <- sequence $ pos <#> \p ->
    map (Tuple p) (lookup2d p xs)
      # Either.note (ErrLineWrongLength { guessedSize: siz, pos: p })
  pure $ UnsafeGrid siz $ Map.fromFoldable mp
  where
  pos = positionsFromSize siz
  siz = guessSize xs

fromArraysAdjustedTo :: forall a. Size -> a -> Array (Array a) -> Grid a
fromArraysAdjustedTo siz def xs =
  fill siz (\pos -> lookup2d pos xs # fromMaybe def)

fromArraysAdjusted :: forall a. a -> Array (Array a) -> Grid a
fromArraysAdjusted def xs = do
  let
    mp = pos <#> \p ->
      Tuple p (lookup2d p xs # fromMaybe def)
  UnsafeGrid siz $ Map.fromFoldable mp
  where
  pos = positionsFromSize siz
  siz = guessSize xs

fromArray :: forall a. Size -> a -> Array a -> Grid a
fromArray siz def xs = UnsafeGrid siz mp
  where
  mp = positionsFromSize siz
    <#> mkEntry
    # Map.fromFoldable
  countRow = getX (toVec siz)
  mkEntry pos@(Pos vec) | x <- getX vec, y <- getY vec =
    Tuple pos $
      Arr.index xs (y * countRow + x) # fromMaybe def

toArrays :: forall a. Grid a -> Array (Array a)
toArrays (UnsafeGrid (Size siz) mp) =
  Arr.range 0 (h - 1) <#> mkLine
  where
  w = getX siz
  h = getY siz
  mkLine y = Arr.range 0 (w - 1) <#> \x -> mkCell x y
  mkCell x y = Map.lookup (Pos $ vec2 x y) mp # fromMaybe'
    \_ -> unsafeCrashWith "Impossible lookup"

rotateClockwise :: forall a. Grid a -> Grid a
rotateClockwise grid = UnsafeGrid newSize newMap
  where
  newSize = let vec = toVec oldSize in Size $ vec2 (getY vec) (getX vec)
  oldSize = getSize grid
  newMap =
    positionsFromSize oldSize
      <#> mkEntry
      # Map.fromFoldable

  maxY = (oldSize # toVec # getY) - 1

  mkEntry pos@(Pos vec) = Tuple (Pos $ vec2 (maxY - getY vec) (getX vec)) $
    unsafePartial
      (lookup pos grid # fromMaybe' (\_ -> unsafeCrashWith "impossible lookup"))

--------------------------------------------------------------------------------
--- Util
--------------------------------------------------------------------------------

range' :: Int -> Array Int
range' end = unfoldr go zero
  where
  go i | i == end = Nothing
  go i = Just (Tuple i (i + 1))

positionsFromSize :: Size -> Array Pos
positionsFromSize (Size siz) = ado
  x <- range' (getX siz)
  y <- range' (getY siz)
  in Pos $ vec2 x y

isInSize :: Pos -> Size -> Boolean
isInSize (Pos pos) (Size siz) =
  inRange (getX pos) (getX siz) && inRange (getY pos) (getY siz)
  where
  inRange :: Int -> Int -> Boolean
  inRange x w = 0 <= x && x < w

guessSize :: forall a. Array (Array a) -> Size
guessSize xs = Size $ vec2 x y
  where
  x = Arr.length <<< fromMaybe [] <<< Arr.head $ xs
  y = Arr.length xs

lookup2d :: forall a. Pos -> Array (Array a) -> Maybe a
lookup2d (Pos vec) = index' (getY vec) >=> index' (getX vec)

index' :: forall a. Int -> Array a -> Maybe a
index' = flip Arr.index