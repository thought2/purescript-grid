module Test.Data.Grid (spec) where

import Prelude

import Control.Monad.Gen (class MonadGen, choose, chooseInt, filtered, oneOf)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Filterable (maybeBool)
import Data.Generic.Rep (class Generic)
import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Traversable (and, minimum)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.QuickCheck (class Arbitrary, class Testable, Result, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec = do
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

        it "returns a correct grid when a valid size was given" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

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
            FnWithArgs1 (\x -> G.fill x (const 0)) size
              `shouldEvalTo1` Nothing

        it "returns a grid of the size that was given" do
          quickCheck \(Valid size) ->
            FnWithArgs1 (\x -> G.size <$> G.fill x (const 0)) size
              `shouldEvalTo1` Just size

        it "returns a grid with the correct value in each cell when given a valid size" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          quickCheck \(Valid size) ->
            FnWithArgs1
              ( \x -> G.fill x fillFn
                  <#> (G.toUnfoldable :: _ -> Array _)
                    >>> map (\(Tuple k v) -> fillFn k == v)
                    >>> and
              )
              size
              `shouldEvalTo1` Just true

      describe "fillTry" do
        it "succeeds for a valid size" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          G.fillTry (Size $ Vec 2 3) fillFn
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

        it "returns an empty grid for a given invalid size (1)" do
          quickCheck \(Invalid size) ->
            FnWithArgs1
              (\x -> G.fillTry x (const 0))
              size
              `shouldEvalTo1` (G.fromArraysConform [])

        it "returns a grid with the correct value in each cell when given a valid size (1)" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          quickCheck \(Valid size) ->
            FnWithArgs1
              ( \x -> G.fillTry x fillFn
                  # (G.toUnfoldable :: _ -> Array _)
                      >>> map (\(Tuple k v) -> fillFn k == v)
                      >>> and
              )
              size
              `shouldEvalTo1` true

      describe "fromArrays" do
        it "returns a correct grid when given a valid size (1) and matching arrays (2)" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual`
              do
                Just $ G.fromArraysConform
                  [ [ 1, 2 ]
                  , [ 3, 4 ]
                  ]

        it "fails when given an invalid size (1) and correct arrays (2)" do
          G.fromArrays (Size $ Vec (-1) 3) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual` Nothing

        it "fails when given arrays (2) that don't match to the size (1)" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1 ], [ 3, 4 ] ]
            `shouldEqual` Nothing

        it "fails when given an invalid size (1) and any arrays (2)" do
          quickCheck \(Invalid size) (Any (Array2d xs)) ->
            FnWithArgs2 G.fromArrays size xs
              `shouldEvalTo2` Nothing

        it "fails when given invalid arrays (2) and any size (1)" do
          quickCheck \(Any size) (Invalid (Array2d xs)) ->
            FnWithArgs2 G.fromArrays size xs
              `shouldEvalTo2` Nothing

        it "fails when given a valid size (1) and valid arrays (2) which don't match the size" do
          quickCheck \(Invalid (Tuple size (Array2d xs))) ->
            FnWithArgs2 G.fromArrays size xs
              `shouldEvalTo2` Nothing

        it "returns a grid of the given valid size (1) and the given arrays (2) which match the size" do
          quickCheck \(Valid (Tuple size (Array2d xs))) ->
            FnWithArgs2 (\x1 x2 -> G.size <$> G.fromArrays x1 x2) size xs
              `shouldEvalTo2` Just size

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

        it "returns an adjusted grid" do
          quickCheck \(Invalid (Array2d xs)) ->
            FnWithArgs1 (G.fromArraysConform >>> G.size) xs
              `shouldEvalTo1`
                ( Size $ Vec
                    (xs <#> Arr.length # minimum # fromMaybe 0)
                    (Arr.length xs)
                )

        it "returns an adjusted grid" do
          quickCheck \(Valid (Array2d xs)) ->
            FnWithArgs1 (G.fromArraysConform >>> G.toArrays) xs
              `shouldEvalTo1` xs

      describe "fromArraysPartial" do
        it "returns a correct grid when given a valid size (1) and arrays (2) that match to the size" do
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
        it "throws an error when given invalid arrays (2) and a valid size (1)" do
          expectError do
            void $ runPartial \_ ->
              G.fromArraysPartial (Size $ Vec 2 3)
                [ [ "0-0", "1-0" ]
                , [ "0-1" ]
                , [ "0-2", "1-2" ]
                ]
        it "throws an error when given an invalid size (1) and valid arrays (2)" do
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
        it "returns a correct grid when a valid size (1) and an array of all entries (2) is given" do
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

        it "fails when an invalid size (1) and any entries (2) are given" do
          quickCheck \(Invalid size) (Any (Entries xs)) ->
            FnWithArgs2 G.fromFoldable size xs
              `shouldEvalTo2` Nothing

        it "fails when any size (1) and invalid entries (2) are given" do
          quickCheck \(Invalid size) (Invalid (Entries xs)) ->
            FnWithArgs2 G.fromFoldable size xs
              `shouldEvalTo2` Nothing

        it "fails when a valid size (1) and entries (2) are given that match the size" do
          quickCheck \(Valid (Tuple size (Entries xs))) ->
            FnWithArgs2 G.fromFoldable size xs
              `shouldEvalTo2` Nothing

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
    [ G.Vec <$> genNegInt <*> genPosInt
    , G.Vec <$> genPosInt <*> genNegInt
    , pure $ G.Vec 1 0
    , pure $ G.Vec 0 1
    ]
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

genValidEntry :: Gen (Tuple Pos Int)
genValidEntry = Tuple <$> genValidPos <*> pure 0

genInvalidEntry :: Gen (Tuple Pos Int)
genInvalidEntry = Tuple <$> genInvalidPos <*> pure 0

-- genValidEntryOfSize :: Size -> Gen (Tuple Pos Int)
-- genValidEntryOfSize = Tuple <$> genInvalidPos <*> pure 0

genValidPos :: Gen Pos
genValidPos = Pos <$> (Vec <$> genPosInt0 <*> genPosInt0)

genValidPosOfSize :: Size -> Gen Pos
genValidPosOfSize (Size (Vec w h)) = Pos <$>
  (Vec <$> chooseInt 0 (w - 1) <*> chooseInt 0 (h - 1))

genInvalidPos :: Gen Pos
genInvalidPos = Pos <$>
  (Vec <$> genNegInt <*> genNegInt)
    `choose` (Vec <$> genNegInt <*> genPosInt0)
    `choose` (Vec <$> genPosInt0 <*> genNegInt)

--genInvalidPos' :: Gen Pos
genInvalidPos' :: Gen Pos
genInvalidPos' = map Pos
  $ oneOf
  $ NEA.cons'
      (Vec <$> genNegInt <*> genNegInt)
      [ Vec <$> genNegInt <*> genPosInt0
      , Vec <$> genPosInt0 <*> genNegInt
      ]

oneOf' :: Partial => forall a. Array (Gen a) -> Gen a
oneOf' = nea >>> oneOf

genInt :: Gen Int
genInt = genNegInt `choose` genPosInt0

genPosInt0 :: Gen Int
genPosInt0 =
  NEA.cons'
    (chooseInt max 3)
    (pure <$> [ 2, 1, 0 ])
    # oneOf

genPosInt0' :: Gen Int
genPosInt0' = oneOf $ NEA.cons'
  (chooseInt max 3)
  [ pure 2
  , pure 1
  , pure 0
  ]

---

nea :: Partial => forall a. Array a -> NonEmptyArray a
nea xs = case NEA.fromArray xs of
  Nothing -> unsafeCrashWith "array empty!"
  Just x -> x

data F3 a b c z = F3 (a -> b -> c -> z) a b c

data FnWithArgs2 a b z = FnWithArgs2 (a -> b -> z) a b

data FnWithArgs1 a z = FnWithArgs1 (a -> z) a

shouldEvalTo2
  :: forall a b z
   . Eq z
  => Show a
  => Show b
  => Show z
  => FnWithArgs2 a b z
  -> z
  -> Result
shouldEvalTo2 (FnWithArgs2 f x1 x2) exp = ret == exp <?> show
  { x1, x2, ret, exp }
  where
  ret = f x1 x2

shouldEvalTo1
  :: forall a z
   . Eq z
  => Show a
  => Show z
  => FnWithArgs1 a z
  -> z
  -> Result
shouldEvalTo1 (FnWithArgs1 f x1) exp = ret == exp <?> show
  { x1, ret, exp }
  where
  ret = f x1

quickCheck_ :: forall a. Testable a => Gen a -> Aff Unit
quickCheck_ = quickCheck

--------------------------------------------------------------------------------
--- Invalid
--------------------------------------------------------------------------------

newtype Invalid a = Invalid a

derive instance Generic (Invalid a) _

derive instance Eq a => Eq (Invalid a)

instance Show a => Show (Invalid a) where
  show = genericShow

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

instance Arbitrary (Invalid Entries) where
  arbitrary = do
    len <- genPosInt
    Invalid <<< Entries <$> replicateA len genInvalidEntry

--------------------------------------------------------------------------------
--- Valid
--------------------------------------------------------------------------------

newtype Valid a = Valid a

derive instance Generic (Valid a) _

derive instance Eq a => Eq (Valid a)

instance Show a => Show (Valid a) where
  show = genericShow

instance Arbitrary (Valid Size) where
  arbitrary = Valid <$> genValidSize

instance Arbitrary (Valid Array2d) where
  arbitrary = Valid <<< Array2d <$> genValid2dArray

instance Arbitrary (Valid (Tuple Size Array2d)) where
  arbitrary = do
    size <- genValidSize
    xs <- pure size >>= genValid2dArrayOfSize
    pure $ Valid (Tuple size (Array2d xs))

instance Arbitrary (Valid Entries) where
  arbitrary = do
    len <- genPosInt
    Valid <<< Entries <$> replicateA len genValidEntry

instance Arbitrary (Valid (Tuple Size Entries)) where
  arbitrary = unsafeCoerce 1

-- do
-- size <- genValidSize
-- xs <- pure size >>= genValid2dArrayOfSize
-- pure $ Valid (Tuple size (Array2d xs))

--------------------------------------------------------------------------------
--- Any
--------------------------------------------------------------------------------

newtype Any a = Any a

derive instance Generic (Any a) _

derive instance Eq a => Eq (Any a)

instance Show a => Show (Any a) where
  show = genericShow

instance Arbitrary (Any Size) where
  arbitrary = Any <$> genSize

instance Arbitrary (Any Array2d) where
  arbitrary = Any <<< Array2d <$> gen2dArray

instance Arbitrary (Any Entries) where
  arbitrary = do
    len <- genPosInt
    Any <<< Entries <$>
      (replicateA len (genValidEntry `choose` genInvalidEntry))

---

newtype Array2d = Array2d (Array (Array Int))

derive instance Generic Array2d _

derive instance Eq Array2d

instance Show Array2d where
  show = genericShow

---

newtype Entries = Entries (Array (Tuple Pos Int))

derive instance Generic Entries _

derive instance Eq Entries

instance Show Entries where
  show = genericShow
