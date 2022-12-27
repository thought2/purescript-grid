module Test.Data.Grid where

import Prelude

import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
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
          G.empty
            `shouldEqual`
              G.fromArraysExtend "" []

      describe "singleton" do
        it "creates an singleton Grid" do
          G.singleton 'A'
            `shouldEqual`
              G.fromArraysExtend ' ' [ [ 'A' ] ]

      describe "fill" do
        it "works for a simple example" do
          G.fill (Size $ Vec 2 3)
            (\(Pos (Vec x y)) -> show x <> "-" <> show y)
            `shouldEqual`
              G.fromArraysExtend ""
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

      describe "fromFlatArrayFitTo" do
        it "works for a simple example" do
          G.fromFlatArrayFitTo (Size $ Vec 2 3) ""
            [ "0-0", "1-0", "0-1", "1-1", "0-2", "1-2" ]
            `shouldEqual`
              G.fromArraysExtend ""
                [ [ "0-0", "1-0" ]
                , [ "0-1", "1-1" ]
                , [ "0-2", "1-2" ]
                ]

    describe "getCell" do
      it "works for a simple example" do
        do
          G.getCell (Pos $ Vec 1 2) $
            G.fromArraysExtend ""
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
            G.fromArraysExtend ""
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
            G.fromArraysExtend ""
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

    describe "toUnfoldable" do
      it "works for a simple example" do
        do
          G.toUnfoldable $
            G.fromArraysExtend ""
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
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            (Size $ Vec 2 3)

    describe "findEntry" do
      it "works for a simple example" do
        do
          G.findEntry (\(Tuple _ v) -> v == "1-1") $
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual` do
            Just $ (Pos $ Vec 1 1) /\ "1-1"

    describe "setCell" do
      it "works for a simple example" do
        do
          G.setCell (Pos $ Vec 1 1) "ABC" $
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual` do
            Just $ G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "ABC" ]
              , [ "0-2", "1-2" ]
              ]

    describe "trySetCell" do
      it "works for a simple example" do
        do
          G.trySetCell (Pos $ Vec 1 1) "ABC" $
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "ABC" ]
              , [ "0-2", "1-2" ]
              ]

    describe "setSubGrid" do
      it "works for a simple example" do
        G.setSubGrid (Pos $ Vec 1 1)
          do
            G.fromArraysExtend ""
              [ [ "AAA", "BBB" ]
              , [ "CCC", "DDD" ]
              ]
          do
            G.fromArraysExtend ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "2-1" ]
              , [ "0-2", "1-2", "2-2" ]
              ]
          `shouldEqual` do
            Just $ G.fromArraysExtend ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "AAA", "BBB" ]
              , [ "0-2", "CCC", "DDD" ]
              ]

    describe "setSubGridClip" do
      it "works for a simple example" do
        G.setSubGridClip (Pos $ Vec 2 1)
          do
            G.fromArraysExtend ""
              [ [ "AAA", "BBB" ]
              , [ "CCC", "DDD" ]
              ]
          do
            G.fromArraysExtend ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "2-1" ]
              , [ "0-2", "1-2", "2-2" ]
              ]
          `shouldEqual`
            G.fromArraysExtend ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "AAA" ]
              , [ "0-2", "1-2", "CCC" ]
              ]

    describe "fromArraysFitTo" do
      it "works for a simple example" do
        G.fromArraysFitTo (Size $ Vec 2 4) "AAA"
          [ [ "0-0", "1-0" ]
          , [ "0-1" ]
          , [ "0-2", "1-2", "2-2" ]
          ]
          `shouldEqual`
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "AAA" ]
              , [ "0-2", "1-2" ]
              , [ "AAA", "AAA" ]
              ]

    describe "fromArraysExtend" do
      it "exists" do
        G.fromArraysExtend ""
          [ [ "0-0", "1-0" ]
          , [ "0-1", "1-1" ]
          , [ "0-2", "1-2" ]
          ]
          `shouldEqual`
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

    describe "toArrays" do
      it "works for a simple example" do
        do
          G.toArrays $
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            [ [ "0-0", "1-0" ]
            , [ "0-1", "1-1" ]
            , [ "0-2", "1-2" ]
            ]

    describe "rotateClockwise" do
      it "works for a simple example" do
        do
          G.rotateClockwise $
            G.fromArraysExtend ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            G.fromArraysExtend ""
              [ [ "0-2", "0-1", "0-0" ]
              , [ "1-2", "1-1", "1-0" ]
              ]