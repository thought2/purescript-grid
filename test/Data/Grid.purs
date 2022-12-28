module Test.Data.Grid where

import Prelude

import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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
        it "works for a simple example" do
          G.fill (Size $ Vec 2 3)
            (\(Pos (Vec x y)) -> show x <> "-" <> show y)
            `shouldEqual`
              G.fromArraysConform
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

      describe "fromArrays" do
        it "works for a simple example" do
          G.fromArrays (Size $ Vec 2 2) [ [ 1, 2 ], [ 3, 4 ] ]
            `shouldEqual`
              do
                Just $ G.fromArraysConform
                  [ [ 1, 2 ]
                  , [ 3, 4 ]
                  ]

      describe "fromArraysConform" do
        it "works for a simple example" do
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

      describe "trySetCell" do
        it "works for a simple example" do
          do
            G.trySetCell (Pos $ Vec 1 1) "ABC" $
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

    describe "Debug Tools" do
      describe "printStringGrid" do
        it "works for a simple example" do
          do
            G.printStringGrid $
              G.fromArraysConform
                [ [ "bird", "dog" ]
                , [ "cat", "horse" ]
                , [ "monkey", "giraffe" ]
                ]
            `shouldEqual` do
              Str.joinWith "\n"
                [ "bird   dog    "
                , "cat    horse  "
                , "monkey giraffe"
                ]