--------------------------------------------------------------------------------
--- Exports
--------------------------------------------------------------------------------

module Test.Data.Grid
  ( Any(..)
  , Array2d(..)
  , Entries(..)
  , Invalid(..)
  , Valid(..)
  , genPosInt0
  , spec
  ) where

--------------------------------------------------------------------------------
--- Imports
--------------------------------------------------------------------------------

import Prelude

import Control.Monad.Gen (choose, chooseInt, filtered, oneOf)
import Data.Array (mapWithIndex)
import Data.Array as Arr
import Data.Array.NonEmpty as NEA
import Data.Filterable (maybeBool)
import Data.Generic.Rep (class Generic)
import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Traversable (and, minimum)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, Result, arbitrary, (<?>))
import Test.QuickCheck.Gen (Gen, vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

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

        it "returns a correct grid when a valid size (1) was given" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          G.fill (Size $ Vec 2 3) fillFn
            `shouldEqual`
              do
                Just $ G.fromArraysConform
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]

        it "fails when an invalid size (1) was given" do
          quickCheck \(Invalid size) ->
            FnWithArgs1 (\x -> G.fill x (const 0)) size
              `shouldEvalTo1` Nothing

        it "returns a grid of the size (1) that was given" do
          quickCheck \(Valid size) ->
            ( FnWithArgs1 (\x -> G.fill x (const 0)) size
                <#> map G.size
            )
              `shouldEvalTo1` Just size

        it "returns a grid with the correct value in each cell when given a valid size (1)" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          quickCheck \(Valid size) ->
            ( FnWithArgs1 (\x -> G.fill x fillFn) size
                <#> map
                  ( (G.toUnfoldable :: _ -> Array _)
                      >>> map (\(Tuple k v) -> fillFn k == v)
                      >>> and
                  )
            )
              `shouldEvalTo1` Just true

      describe "fillTry" do
        it "succeeds for a valid size (1)" do
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
            FnWithArgs1 (\x -> G.fillTry x (const 0)) size
              `shouldEvalTo1`
                G.fromArraysConform []

        it "returns a grid with the correct value in each cell when given a valid size (1)" do
          let fillFn (Pos (Vec x y)) = show x <> "-" <> show y

          quickCheck \(Valid size) ->
            ( FnWithArgs1 (\x -> G.fillTry x fillFn) size
                <#>
                  ( (G.toUnfoldable :: _ -> Array _)
                      >>> map (\(Tuple k v) -> fillFn k == v)
                      >>> and
                  )
            )
              `shouldEvalTo1` true

      describe "fromArrays" do
        it "returns a correct grid when given a valid size (1) and matching arrays (2)" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual`
              ( Just $ G.fromArraysConform
                  [ [ 1, 2 ]
                  , [ 3, 4 ]
                  ]
              )

        it "fails when given an invalid size (1) and valid arrays (2)" do
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

        it "fails when given an invalid pair of size (1) and arrays (2)" do
          quickCheck \(Invalid (Tuple (Valid size) (Valid (Array2d xs)))) ->
            FnWithArgs2 G.fromArrays size xs
              `shouldEvalTo2` Nothing

        it "returns a grid of the given a valid combination of size (1) arrays (2)" do
          quickCheck \(Valid (Tuple size (Array2d xs))) ->
            FnWithArgs2 (\x1 x2 -> G.size <$> G.fromArrays x1 x2) size xs
              `shouldEvalTo2` Just size

      describe "fromArraysConform" do
        it "succeeds for valid arrays (1)" do
          G.fromArraysConform
            [ [ "0-0", "1-0" ]
            , [ "0-1", "1-1" ]
            , [ "0-2", "1-2" ]
            ]
            `shouldEqual`
              ( unsafePartial $ G.fromArraysPartial (Size $ Vec 2 3)
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "1-1" ]
                  , [ "0-2", "1-2" ]
                  ]
              )

        it "succeeds for invalid arrays (1)" do
          G.fromArraysConform
            [ [ "0-0", "1-0" ]
            , [ "0-1" ]
            , [ "0-2", "1-2" ]
            ]
            `shouldEqual`
              ( unsafePartial $ G.fromArraysPartial (Size $ Vec 1 3)
                  [ [ "0-0" ]
                  , [ "0-1" ]
                  , [ "0-2" ]
                  ]
              )

        it "returns an adjusted grid when invalid arrays (1) are given" do
          quickCheck \(Invalid (Array2d xs)) ->
            FnWithArgs1 (G.fromArraysConform >>> G.size) xs
              `shouldEvalTo1`
                ( let
                    w = xs <#> Arr.length # minimum # fromMaybe 0
                    h = if w == 0 then 0 else (Arr.length xs)
                  in
                    Size $ Vec w h
                )

        it "returns an grid equivalent to the given arrays (1)" do
          quickCheck \(Valid (Array2d xs)) ->
            ( FnWithArgs1 G.fromArraysConform xs
                <#> G.toArrays
            )
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
              ( Just $ G.fromArraysConform
                  [ [ 1, 3 ]
                  , [ 2, 4 ]
                  ]
              )

        it "fails when an invalid size (1) and any entries (2) are given" do
          quickCheck \(Invalid size) (Any (Entries xs)) ->
            FnWithArgs2 G.fromFoldable size xs
              `shouldEvalTo2` Nothing

        it "fails when any size (1) and invalid entries (2) are given" do
          quickCheck \(Any size) (Invalid (Entries xs)) ->
            FnWithArgs2 G.fromFoldable size xs
              `shouldEvalTo2` Nothing

        it "returns a grid when an invalid combination of size (1) and entries (2) are given" do
          quickCheck \(Invalid (Tuple (Valid size) (Valid (Entries xs)))) ->
            ( FnWithArgs2 G.fromFoldable size xs
                <#> map G.size
            )
              `shouldEvalTo2` Nothing

        it "returns a grid when a valid combination of size (1) and entries (2) are given" do
          quickCheck \(Valid (Tuple size (Entries xs))) ->
            ( FnWithArgs2 G.fromFoldable size xs
                <#> map G.size
            )
              `shouldEvalTo2` Just size

    describe "Destructors" do
      describe "toArrays" do
        it "works for a simple example" do
          ( G.toArrays $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

      describe "toUnfoldable" do
        it "works for a simple example" do
          ( G.toUnfoldable $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
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
          ( G.size $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              (Size $ Vec 2 3)

      describe "findEntry" do
        it "works for a simple example" do
          ( G.findEntry (\_ v -> v == "1-1") $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              (Just $ (Pos $ Vec 1 1) /\ "1-1")

      describe "getCell" do
        it "works for a simple example" do
          ( G.getCell (Pos $ Vec 1 2) $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              Just "1-2"

      describe "getCellModulo" do
        it "works for a simple example" do
          ( G.getCellModulo (Pos $ Vec 2 3) $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              "0-0"

      describe "positions" do
        it "works for a simple example" do
          ( G.positions $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
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
          ( G.rotateClockwise $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-2", "0-1", "0-0" ]
                , [ "1-2", "1-1", "1-0" ]
                ]

    describe "SubGrid Modifiers" do
      describe "setSubGrid" do
        it "works for a simple example" do
          G.setSubGrid (Pos $ Vec 1 1)
            ( G.fromArraysConform
                [ [ "AAA", "BBB" ]
                , [ "CCC", "DDD" ]
                ]
            )
            ( G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "2-1" ]
                , [ "0-2", "1-2", "2-2" ]
                ]
            )
            `shouldEqual`
              ( Just $ G.fromArraysConform
                  [ [ "0-0", "1-0", "2-0" ]
                  , [ "0-1", "AAA", "BBB" ]
                  , [ "0-2", "CCC", "DDD" ]
                  ]
              )

      describe "setSubGridClip" do
        it "works for a simple example" do
          G.setSubGridClip (Pos $ Vec 2 1)
            ( G.fromArraysConform
                [ [ "AAA", "BBB" ]
                , [ "CCC", "DDD" ]
                ]
            )
            ( G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "2-1" ]
                , [ "0-2", "1-2", "2-2" ]
                ]
            )
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0", "2-0" ]
                , [ "0-1", "1-1", "AAA" ]
                , [ "0-2", "1-2", "CCC" ]
                ]

    describe "Cell Modifiers" do
      describe "setCell" do
        it "works for a simple example" do
          ( G.setCell (Pos $ Vec 1 1) "ABC" $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
            `shouldEqual`
              ( Just $ G.fromArraysConform
                  [ [ "0-0", "1-0" ]
                  , [ "0-1", "ABC" ]
                  , [ "0-2", "1-2" ]
                  ]
              )

      describe "setCellTry" do
        it "works for a simple example" do
          ( G.setCellTry (Pos $ Vec 1 1) "ABC" $
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]
          )
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
          ( G.printGrid_ $
              G.fromArraysConform
                [ [ "bird", "dog" ]
                , [ "cat", "horse" ]
                , [ "monkey", "giraffe" ]
                ]
          )
            `shouldEqual`
              ( Str.joinWith "\n"
                  [ "bird    dog    "
                  , "cat     horse  "
                  , "monkey  giraffe"
                  ]
              )

      describe "printGrid" do
        it "works for a simple example" do
          ( G.printGrid
              { formatCell: G.padRight '.'
              , colSep: "|"
              , rowSep: "\n\n"
              }
              ( G.fromArraysConform
                  [ [ "bird", "dog" ]
                  , [ "cat", "horse" ]
                  , [ "monkey", "giraffe" ]
                  ]
              )
          )
            `shouldEqual`
              ( Str.joinWith "\n"
                  [ "bird...|dog...."
                  , ""
                  , "cat....|horse.."
                  , ""
                  , "monkey.|giraffe"
                  ]
              )

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
--- Util
--------------------------------------------------------------------------------
runPartial :: forall a. (Unit -> Partial => a) -> Aff a
runPartial p = pure unit >>= \_ -> pure $ unsafePartial $ p unit

--------------------------------------------------------------------------------
--- FnWithArgs
--------------------------------------------------------------------------------

data FnWithArgs3 a b c z = F3 (a -> b -> c -> z) a b c

derive instance Functor (FnWithArgs3 a b c)

data FnWithArgs2 a b z = FnWithArgs2 (a -> b -> z) a b

derive instance Functor (FnWithArgs2 a b)

data FnWithArgs1 a z = FnWithArgs1 (a -> z) a

derive instance Functor (FnWithArgs1 a)

shouldEvalTo2
  :: forall a b z
   . Eq z
  => Show a
  => Show b
  => Show z
  => (FnWithArgs2 a b z -> z -> Result)
shouldEvalTo2 (FnWithArgs2 f x1 x2) expected =
  actual == expected <?> printEvalResult
    { actual: show actual
    , expected: show expected
    , args: [ show x1, show x2 ]
    }
  where
  actual = f x1 x2

shouldEvalTo1
  :: forall a z
   . Eq z
  => Show a
  => Show z
  => (FnWithArgs1 a z -> z -> Result)
shouldEvalTo1 (FnWithArgs1 f x1) expected =
  actual == expected <?> printEvalResult
    { actual: show actual
    , expected: show expected
    , args: [ show x1 ]
    }
  where
  actual = f x1

type EvalResult =
  { args :: Array String
  , expected :: String
  , actual :: String
  }

printEvalResult :: EvalResult -> String
printEvalResult result = "\n" <> Str.joinWith "\n"
  (args <> [ actual ] <> [ expected ])
  where
  args = mapWithIndex mkArg result.args
  mkArg n x = "Input " <> show (n + 1) <> ": " <> x
  actual = "Actual: " <> result.actual
  expected = "Expected: " <> result.expected

--------------------------------------------------------------------------------
--- Gen
--------------------------------------------------------------------------------

maxInt :: Int
maxInt = 10

genDifferent :: forall a. Eq a => Gen a -> Gen (Tuple a a)
genDifferent gen = do
  x <- gen
  y <- gen <#> maybeBool (_ /= x) # filtered
  pure $ Tuple x y

genValid2dArray :: Valid Size -> Gen (Valid Array2d)
genValid2dArray (Valid (Size (Vec width height))) =
  (pure width >>= genLine)
    # vectorOf height
    <#> Array2d >>> Valid

genValidEntries :: Valid Size -> Gen (Valid Entries)
genValidEntries (Valid (Size (Vec width height))) =
  pure $ Valid $ Entries ado
    x <- Arr.dropEnd 1 $ Arr.range 0 width
    y <- Arr.dropEnd 1 $ Arr.range 0 height
    in Tuple (Pos (Vec x y)) 0

allSameLength :: forall a. Array (Array a) -> Boolean
allSameLength xs =
  xs
    <#> Arr.length
    # Arr.nubEq
    # Arr.length
    # (_ == 1)

genLine :: Int -> Gen (Array Int)
genLine w = vectorOf w (pure 0)

genNegInt :: Gen Int
genNegInt = oneOf
  ( NEA.cons'
      (chooseInt (-maxInt) (-3))
      [ pure (-2)
      , pure (-1)
      ]
  )

genPosInt :: Gen Int
genPosInt = oneOf
  ( NEA.cons'
      (chooseInt maxInt 3)
      [ pure 2
      , pure 1
      ]
  )

genPosInt0 :: Gen Int
genPosInt0 = oneOf
  ( NEA.cons'
      (chooseInt maxInt 3)
      [ pure 2
      , pure 1
      , pure 0
      ]
  )

genValidEntry :: Valid Size -> Gen (Tuple Pos Int)
genValidEntry size = ado
  Valid pos <- genValidPos size
  in
    Tuple pos 0

genInvalidEntry :: Gen (Tuple Pos Int)
genInvalidEntry =
  Tuple <$> (un Invalid <$> arbitrary) <*> pure 0

genValidPos :: Valid Size -> Gen (Valid Pos)
genValidPos (Valid (Size (Vec w h))) =
  Valid <<< Pos <$>
    ( Vec
        <$> chooseInt 0 (w - 1)
        <*> chooseInt 0 (h - 1)
    )

--------------------------------------------------------------------------------
--- Invalid
--------------------------------------------------------------------------------

newtype Invalid a = Invalid a

derive instance Generic (Invalid a) _

derive instance Newtype (Invalid a) _

derive instance Eq a => Eq (Invalid a)

instance Show a => Show (Invalid a) where
  show = genericShow

instance Arbitrary (Invalid Size) where
  arbitrary =
    ( NEA.cons'
        (G.Vec <$> genNegInt <*> genNegInt)
        [ G.Vec <$> genNegInt <*> genPosInt
        , G.Vec <$> genPosInt <*> genNegInt
        , pure $ G.Vec 1 0
        , pure $ G.Vec 0 1
        ]
    )
      # oneOf
      <#> G.Size >>> Invalid

instance Arbitrary (Invalid Pos) where
  arbitrary =
    ( NEA.cons'
        (Vec <$> genNegInt <*> genNegInt)
        [ Vec <$> genNegInt <*> genPosInt0
        , Vec <$> genPosInt0 <*> genNegInt
        ]
    )
      # oneOf
      <#> Pos >>> Invalid

instance Arbitrary (Invalid Array2d) where
  arbitrary =
    do
      height <- genPosInt
        <#> maybeBool (_ /= 1)
        # filtered
      entries <-
        (genPosInt0 >>= genLine)
          # vectorOf height
          <#> maybeBool (not <<< allSameLength)
          # filtered
      pure
        (Invalid $ Array2d entries)

instance Arbitrary (Invalid (Tuple (Valid Size) (Valid Array2d))) where
  arbitrary = do
    Tuple size otherSize <- genDifferent arbitrary

    xs <- genValid2dArray otherSize

    pure
      (Invalid $ Tuple size xs)

instance Arbitrary (Invalid (Tuple (Valid Size) (Valid Entries))) where
  arbitrary = do
    Tuple size otherSize <- genDifferent arbitrary

    xs <- genValidEntries otherSize

    pure
      (Invalid $ Tuple size xs)

instance Arbitrary (Invalid Entries) where
  arbitrary = do
    len <- genPosInt
    genInvalidEntry
      # vectorOf len
      <#> Entries >>> Invalid

--------------------------------------------------------------------------------
--- Valid
--------------------------------------------------------------------------------

newtype Valid a = Valid a

derive instance Generic (Valid a) _

derive instance Newtype (Valid a) _

derive instance Eq a => Eq (Valid a)

instance Show a => Show (Valid a) where
  show = genericShow

instance Arbitrary (Valid Size) where
  arbitrary =
    ( NEA.cons'
        (Vec <$> genPosInt <*> genPosInt)
        [ pure $ Vec 0 0 ]
    )
      # oneOf
      <#> Size >>> Valid

instance Arbitrary (Valid Pos) where
  arbitrary =
    (Vec <$> genPosInt0 <*> genPosInt0)
      <#> Pos >>> Valid

instance Arbitrary (Valid Array2d) where
  arbitrary =
    do
      validSize <- arbitrary
      genValid2dArray validSize

instance Arbitrary (Valid (Tuple Size Array2d)) where
  arbitrary = do
    validSize@(Valid size) <- arbitrary
    Valid xs <- pure validSize >>= genValid2dArray
    pure $
      Valid (Tuple size xs)

instance Arbitrary (Valid Entries) where
  arbitrary = do
    len <- genPosInt
    size <- arbitrary
    vectorOf len (genValidEntry size)
      <#> Entries >>> Valid

instance Arbitrary (Valid (Tuple Size Entries)) where
  arbitrary = do
    validSize <- arbitrary
    entries <- genValidEntries validSize
    pure $ Valid (Tuple (unwrap validSize) (unwrap entries))

--------------------------------------------------------------------------------
--- Any
--------------------------------------------------------------------------------

newtype Any a = Any a

derive instance Generic (Any a) _

derive instance Newtype (Any a) _

derive instance Eq a => Eq (Any a)

instance Show a => Show (Any a) where
  show = genericShow

instance Arbitrary (Any Size) where
  arbitrary = Any <$> choose
    (un Valid <$> arbitrary)
    (un Invalid <$> arbitrary)

instance Arbitrary (Any Array2d) where
  arbitrary = Any <$> choose
    (un Invalid <$> arbitrary)
    (un Valid <$> arbitrary)

instance Arbitrary (Any Entries) where
  arbitrary = do
    len <- genPosInt
    size <- arbitrary
    (vectorOf len (genValidEntry size `choose` genInvalidEntry))
      <#> Entries >>> Any

--------------------------------------------------------------------------------
--- Array2d
--------------------------------------------------------------------------------

newtype Array2d = Array2d (Array (Array Int))

derive instance Generic Array2d _

derive instance Eq Array2d

instance Show Array2d where
  show = genericShow

--------------------------------------------------------------------------------
--- Entries
--------------------------------------------------------------------------------

newtype Entries = Entries (Array (Tuple Pos Int))

derive instance Generic Entries _

derive instance Eq Entries

instance Show Entries where
  show = genericShow
