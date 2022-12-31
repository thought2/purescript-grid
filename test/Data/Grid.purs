module Test.Data.Grid where

import Prelude

import Control.Monad.Gen (class MonadGen, choose, chooseInt, filtered, oneOf)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Arr
import Data.Array.NonEmpty as NEA
import Data.Filterable (maybeBool)
import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Traversable (and, minimum)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, class Testable, Result, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "Data.Grid" do
    describe "Types" do
      describe "Grid" do
        it "exists" do
          let
            _ = Proxy :: _ (G.Grid Unit)
          pure unit

      describe "Pos" do
        it "can be constructed and destructured" do
          let
            G.Pos (Vec _ _) = G.Pos (Vec 1 2)
          pure unit

      describe "Size" do
        it "can be constructed and destructured" do
          let
            G.Size (Vec _ _) = G.Size (Vec 1 2)
          pure unit

    describe "Constructors" do
      describe "empty" do
        it "creates an empty Grid" do
          (G.empty :: G.Grid Int)
            `shouldEqual`
              G.fromArraysConform []

      describe "singleton" do
        it "creates an singleton Grid" do
          G.singleton 'A'
            `shouldEqual`
              G.fromArraysConform [ [ 'A' ] ]

      describe "fill" do
        let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

        it "returns a correct grid when a valid size was given" do
          G.fill (Size $ Vec 2 3) fillFn
            `shouldEqual`
              do
                Just $ G.fromArraysConform
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]

        it "fails when an invalid size was given" do
          quickCheck \(Invalid size) ->
            F1 (\x -> G.fill x fillFn) size
              `testF1` Nothing

        it "returns a grid of the size that was given" do
          quickCheck \(Valid size) ->
            F1 (\x -> G.size <$> G.fill x fillFn) size
              `testF1` Just size

        it "returns a grid with the correct value in each cell when given a valid size" do
          quickCheck \(Valid size) ->
            F1
              ( \x -> G.fill x fillFn
                  <#> (G.toUnfoldable :: _ -> Array _)
                    >>> map (\(Tuple k v) -> fillFn k == v)
                    >>> and
              )
              size
              `testF1` Just true

      describe "fillTry" do
        let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

        it "succeeds for a valid size" do
          G.fillTry (Size $ Vec 2 3) fillFn
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

        it "returns an empty grid for a given invalid size" do
          quickCheck \(Invalid size) ->
            F1
              (\x -> G.fillTry x fillFn)
              size
              `testF1` (G.fromArraysConform [])

        it "returns a grid with the correct value in each cell when given a valid size" do
          quickCheck \(Valid size) ->
            F1
              ( \x -> G.fillTry x fillFn
                  # (G.toUnfoldable :: _ -> Array _)
                      >>> map (\(Tuple k v) -> fillFn k == v)
                      >>> and
              )
              size
              `testF1` true

      describe "fromArrays" do
        it "returns a correct grid when given a valid size and matching arrays" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual`
              do
                Just $ G.fromArraysConform
                  [ [ 1, 2 ]
                  , [ 3, 4 ]
                  ]

        it "fails when given an invalid size" do
          G.fromArrays (Size $ Vec (-1) 3) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual` Nothing

        it "fails when given arrays that don't match to the size" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1 ], [ 3, 4 ] ]
            `shouldEqual` Nothing

        it "fails when given an invalid size" do
          quickCheck \(Invalid size) (Any (Array2d xs)) ->
            F2 G.fromArrays size xs
              `testF2` Nothing

        it "fails when given invalid arrays" do
          quickCheck \(Any size) (Invalid (Array2d xs)) ->
            F2 G.fromArrays size xs
              `testF2` Nothing

        it "fails when given a valid size and valid arrays which don't match the size" do
          quickCheck \(Invalid (Tuple size (Array2d xs))) ->
            F2 G.fromArrays size xs
              `testF2` Nothing

        it "returns a grid of the given valid size and arrays which match the size" do
          quickCheck \(Valid (Tuple size (Array2d xs))) ->
            F2 (\x1 x2 -> G.size <$> G.fromArrays x1 x2) size xs
              `testF2` Just size

      describe "fromArraysConform" do
        it "succeeds for valid arrays" do
          G.fromArraysConform
            [ [ "0-0", "1-0" ]
            , [ "0-1", "1-1" ]
            , [ "0-2", "1-2" ]
            ]
            `shouldEqual`
              do
                unsafePartial $ G.fromArraysPartial (Size $ Vec 2 3)
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]

        it "succeeds for invalid arrays" do
          G.fromArraysConform
            [ [ "0-0", "1-0" ]
            , [ "0-1" ]
            , [ "0-2", "1-2" ]
            ]
            `shouldEqual`
              do
                unsafePartial $ G.fromArraysPartial (Size $ Vec 1 3)
                  [ [ "0-0" ]
                  , [ "0-1" ]
                  , [ "0-2" ]
                  ]

      describe "fuzzy" do
        it "succeds with an adjusted grid" do
          quickCheck_ do
            xs <- genInvalid2dArray
            pure $ (F1 (G.size <<< G.fromArraysConform) xs) `testF1`
              ( Size $ Vec
                  (xs <#> Arr.length # minimum # fromMaybe 0)
                  (Arr.length xs)
              )
        it "b" do
          quickCheck_ do
            xs <- genValid2dArray
            pure $ (F1 (G.toArrays <<< G.fromArraysConform) xs) `testF1`
              xs

      describe "fromArraysPartial" do
        it "works for a simple example" do
          unsafePartial $
            G.fromArraysPartial (Size $ Vec 2 3)
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
              `shouldEqual`
                G.fromArraysConform
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]
        it "works for a simple example" do
          expectError do
            void $ runPartial \_ ->
              G.fromArraysPartial (Size $ Vec 2 3)
                [ [ "0-0", "1-0" ]
                , [ "0-1" ]
                , [ "0-2", "1-2" ]
                ]
        it "works for a simple example" do
          expectError do
            void $ runPartial \_ ->
              G.fromArraysPartial (Size $ Vec (-1) 3)
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

      describe "mkGrid" do
        it "works for a simple example" do
          unsafePartial $
            G.mkGrid (Size $ Vec 2 3)
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
              `shouldEqual`
                G.fromArraysConform
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]

      describe "fromFoldable" do
        it "works for a simple example" do
          unsafePartial $
            G.fromFoldable (Size $ Vec 2 2)
              [ (Pos $ Vec 0 0) /\ 1
              , (Pos $ Vec 0 1) /\ 2
              , (Pos $ Vec 1 0) /\ 3
              , (Pos $ Vec 1 1) /\ 4
              ]
              `shouldEqual`
                do
                  Just $ G.fromArraysConform
                    [ [ 1, 3 ]
                    , [ 2, 4 ]
                    ]

    describe "Destructors" do
      describe "toArrays" do
        it "works for a simple example" do
          do
            G.toArrays $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

      describe "toUnfoldable" do
        it "works for a simple example" do
          do
            G.toUnfoldable $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              [ (Pos $ Vec 0 0) /\ "0-0"
              , (Pos $ Vec 0 1) /\ "0-1"
              , (Pos $ Vec 0 2) /\ "0-2"
              , (Pos $ Vec 1 0) /\ "1-0"
              , (Pos $ Vec 1 1) /\ "1-1"
              , (Pos $ Vec 1 2) /\ "1-2"
              ]

      describe "size" do
        it "works for a simple example" do
          do
            G.size $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              (Size $ Vec 2 3)

      describe "findEntry" do
        it "works for a simple example" do
          do
            G.findEntry (\_ v -> v == "1-1") $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual` do
              Just $ (Pos $ Vec 1 1) /\ "1-1"

      describe "getCell" do
        it "works for a simple example" do
          do
            G.getCell (Pos $ Vec 1 2) $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              Just "1-2"

      describe "getCellModulo" do
        it "works for a simple example" do
          do
            G.getCellModulo (Pos $ Vec 2 3) $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              "0-0"

      describe "positions" do
        it "works for a simple example" do
          do
            G.positions $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              [ Pos $ Vec 0 0
              , Pos $ Vec 0 1
              , Pos $ Vec 0 2
              , Pos $ Vec 1 0
              , Pos $ Vec 1 1
              , Pos $ Vec 1 2
              ]

    describe "Grid Modifiers" do
      describe "rotateClockwise" do
        it "works for a simple example" do
          do
            G.rotateClockwise $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-2", "0-1", "0-0" ]
                , [ "1-2", "1-1", "1-0" ]
                ]
    describe "SubGrid Modifiers" do

      describe "setSubGrid" do
        it "works for a simple example" do
          G.setSubGrid (Pos $ Vec 1 1)
            do
              G.fromArraysConform
                [ [ "AAA", "BBB" ]
                , [ "CCC", "DDD" ]
                ]
            do
              G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "2-1" ]
                , [ "0-2", "1-2", "2-2" ]
                ]
            `shouldEqual` do
              Just $ G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "AAA", "BBB" ]
                , [ "0-2", "CCC", "DDD" ]
                ]

      describe "setSubGridClip" do
        it "works for a simple example" do
          G.setSubGridClip (Pos $ Vec 2 1)
            do
              G.fromArraysConform
                [ [ "AAA", "BBB" ]
                , [ "CCC", "DDD" ]
                ]
            do
              G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "2-1" ]
                , [ "0-2", "1-2", "2-2" ]
                ]
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "AAA" ]
                , [ "0-2", "1-2", "CCC" ]
                ]

    describe "Cell Modifiers" do
      describe "setCell" do
        it "works for a simple example" do
          do
            G.setCell (Pos $ Vec 1 1) "ABC" $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual` do
              Just $ G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "ABC" ]
                , [ "0-2", "1-2" ]
                ]

      describe "setCellTry" do
        it "works for a simple example" do
          do
            G.setCellTry (Pos $ Vec 1 1) "ABC" $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "ABC" ]
                , [ "0-2", "1-2" ]
                ]

    describe "Pretty Printing" do
      describe "CellFormatter" do
        it "can be constructed and destructured" do
          let
            G.CellFormatter _ = G.CellFormatter \_ -> identity
          pure unit

      describe "printGrid_" do
        it "works for a simple example" do
          do
            G.printGrid_ $
              G.fromArraysConform
                [ [ "bird", "dog" ]
                , [ "cat", "horse" ]
                , [ "monkey", "giraffe" ]
                ]
            `shouldEqual` do
              Str.joinWith "\n"
                [ "bird    dog    "
                , "cat     horse  "
                , "monkey  giraffe"
                ]

      describe "printGrid" do
        it "works for a simple example" do
          do
            G.printGrid
              { formatCell: G.padRight '.'
              , colSep: "|"
              , rowSep: "\n\n"
              } $
              G.fromArraysConform
                [ [ "bird", "dog" ]
                , [ "cat", "horse" ]
                , [ "monkey", "giraffe" ]
                ]
            `shouldEqual` do
              Str.joinWith "\n"
                [ "bird...|dog...."
                , ""
                , "cat....|horse.."
                , ""
                , "monkey.|giraffe"
                ]

      describe "padRight" do
        it "works for a simple example" do
          do
            (unwrap $ G.padRight '.') 10 "Hello"
            `shouldEqual` "Hello....."

      describe "padLeft" do
        it "works for a simple example" do
          do
            (unwrap $ G.padLeft '.') 10 "Hello"
            `shouldEqual` ".....Hello"

--------------------------------------------------------------------------------
--- 
--------------------------------------------------------------------------------
runPartial :: forall a. (Unit -> Partial => a) -> Aff a
runPartial p = pure unit >>= \_ -> pure $ unsafePartial $ p unit

--------------------------------------------------------------------------------
--- 
--------------------------------------------------------------------------------

sizeIsValid :: Size -> Boolean
sizeIsValid (Size (Vec 0 0)) = true
sizeIsValid (Size (Vec x y)) | x >= 1 && y >= 1 = true
sizeIsValid _ = false

--------------------------------------------------------------------------------
--- Gen
--------------------------------------------------------------------------------

gen2dArray :: forall m. MonadGen m => MonadRec m => m (Array (Array Int))
gen2dArray = choose genValid2dArray genInvalid2dArray

genValid2dArray :: forall m. MonadGen m => m (Array (Array Int))
genValid2dArray = do
  size <- genValidSize
  genValid2dArrayOfSize size

genValid2dArrayOfSize :: forall m. MonadGen m => Size -> m (Array (Array Int))
genValid2dArrayOfSize (Size (Vec width height)) = do
  replicateA height (pure width >>= genLine)

genInvalid2dArray :: forall m. MonadGen m => MonadRec m => m (Array (Array Int))
genInvalid2dArray =
  do
    height <- genPosInt
    replicateA height (genPosInt >>= genLine)
    <#> maybeBool (not <<< allSameLength)
    # filtered

allSameLength :: forall a. Array (Array a) -> Boolean
allSameLength xs = xs <#> Arr.length # Arr.nubEq # Arr.length # (_ == 1)

genLine :: forall m. MonadGen m => Int -> m (Array Int)
genLine w = do
  --w' <- choose (pure w) (chooseInt 0 w)
  replicateA w (pure 0)

genSize :: forall m. MonadGen m => MonadRec m => m Size
genSize = choose genValidSize genInvalidSize

genValidSize :: forall m. MonadGen m => m Size
genValidSize =
  NEA.cons'
    (G.Vec <$> genPosInt <*> genPosInt)
    (pure <$> [ G.Vec 0 0 ])
    # oneOf
    <#> G.Size

genInvalidSize :: forall m. MonadGen m => MonadRec m => m Size
genInvalidSize =
  NEA.cons'
    (G.Vec <$> genNegInt <*> genNegInt)
    (pure <$> [ G.Vec 1 0, G.Vec 0 1 ])
    # oneOf
    <#> G.Size

max :: Int
max = 10

genNegInt :: forall m. MonadGen m => m Int
genNegInt =
  NEA.cons'
    (chooseInt (-max) (-3))
    (pure <$> [ -2, -1 ])
    # oneOf

genPosInt :: forall m. MonadGen m => m Int
genPosInt =
  NEA.cons'
    (chooseInt max 3)
    (pure <$> [ 2, 1 ])
    # oneOf

-- ado
--   width <- chooseInt (-100) (-2) `choose` pure (-1)
--   height <- chooseInt (-100) (-2) `choose` pure (-1)

--   in G.Vec width height

removeValues :: forall a m. MonadGen m => MonadRec m => (a -> Boolean) -> m a -> m a
removeValues pred ma = ma
  <#> (\x -> if pred x then Nothing else Just x)
  # filtered

genEdgeInt :: forall m. MonadGen m => m Int
genEdgeInt =
  oneOf
    $ NEA.cons' (chooseInt 0 100)
    $ pure <$> [ -2, -1, 0, 1, 2 ]

genEdgeIntPositive :: forall m. MonadGen m => m Int
genEdgeIntPositive =
  oneOf
    $ NEA.cons' (chooseInt 0 100)
    $ pure <$> [ 0, 1, 2, 3, 4 ]

---

data F3 a b c z = F3 (a -> b -> c -> z) a b c

data F2 a b z = F2 (a -> b -> z) a b

data F1 a z = F1 (a -> z) a

testF2
  :: forall a b z
   . Eq z
  => Show a
  => Show b
  => Show z
  => F2 a b z
  -> z
  -> Result
testF2 (F2 f x1 x2) exp = ret == exp <?> show
  { x1, x2, ret, exp }
  where
  ret = f x1 x2

testF1
  :: forall a z
   . Eq z
  => Show a
  => Show z
  => F1 a z
  -> z
  -> Result
testF1 (F1 f x1) exp = ret == exp <?> show
  { x1, ret, exp }
  where
  ret = f x1

quickCheck_ :: forall a. Testable a => Gen a -> Aff Unit
quickCheck_ = quickCheck

--------------------------------------------------------------------------------
--- Invalid
--------------------------------------------------------------------------------

newtype Invalid a = Invalid a

instance Arbitrary (Invalid Size) where
  arbitrary = Invalid <$> genInvalidSize

instance Arbitrary (Invalid Array2d) where
  arbitrary = Invalid <<< Array2d <$> genInvalid2dArray

instance Arbitrary (Invalid (Tuple Size Array2d)) where
  arbitrary = do
    size <- genValidSize
    xs <- genValidSize
      <#> maybeBool (_ /= size)
      # filtered
      >>= genValid2dArrayOfSize
    pure $ Invalid (Tuple size (Array2d xs))

--------------------------------------------------------------------------------
--- Valid
--------------------------------------------------------------------------------

newtype Valid a = Valid a

instance Arbitrary (Valid Size) where
  arbitrary = Valid <$> genValidSize

instance Arbitrary (Valid Array2d) where
  arbitrary = Valid <<< Array2d <$> genValid2dArray

instance Arbitrary (Valid (Tuple Size Array2d)) where
  arbitrary = do
    size <- genValidSize
    xs <- pure size >>= genValid2dArrayOfSize
    pure $ Valid (Tuple size (Array2d xs))

--------------------------------------------------------------------------------
--- Any
--------------------------------------------------------------------------------

newtype Any a = Any a

instance Arbitrary (Any Size) where
  arbitrary = Any <$> genSize

instance Arbitrary (Any Array2d) where
  arbitrary = Any <<< Array2d <$> gen2dArray

---

newtype Array2d = Array2d (Array (Array Int))

