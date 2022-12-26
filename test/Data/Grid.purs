module Test.Data.Grid where

import Prelude

import Data.Grid (Pos(..), Size(..), Vec(..))
import Data.Grid as G
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Grid" do
    describe "fill" do
      it "works for a simple example" do
        G.fill (Size $ Vec 2 3)
          (\(Pos (Vec x y)) -> show x <> "-" <> show y)
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

    describe "empty" do
      it "creates an empty Grid" do
        G.empty
          `shouldEqual`
            G.fromArraysAdjusted "" []

    describe "getCell" do
      it "works for a simple example" do
        do
          G.getCell (Pos $ Vec 1 2) $
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual` do
            Just $ G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "ABC" ]
              , [ "0-2", "1-2" ]
              ]

    describe "trySetCell" do
      it "works for a simple example" do
        do
          G.trySetCell (Pos $ Vec 1 1) "ABC" $
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "ABC" ]
              , [ "0-2", "1-2" ]
              ]

    describe "setGrid" do
      it "works for a simple example" do
        G.setGrid (Pos $ Vec 1 1)
          do
            G.fromArraysAdjusted ""
              [ [ "AAA", "BBB" ]
              , [ "CCC", "DDD" ]
              ]
          do
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "2-1" ]
              , [ "0-2", "1-2", "2-2" ]
              ]
          `shouldEqual` do
            Just $ G.fromArraysAdjusted ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "AAA", "BBB" ]
              , [ "0-2", "CCC", "DDD" ]
              ]

    describe "setGridCropped" do
      it "works for a simple example" do
        G.setGridCropped (Pos $ Vec 2 1)
          do
            G.fromArraysAdjusted ""
              [ [ "AAA", "BBB" ]
              , [ "CCC", "DDD" ]
              ]
          do
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "2-1" ]
              , [ "0-2", "1-2", "2-2" ]
              ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0", "2-0" ]
              , [ "0-1", "1-1", "AAA" ]
              , [ "0-2", "1-2", "CCC" ]
              ]

    describe "fromArraysAdjustedTo" do
      it "works for a simple example" do
        G.fromArraysAdjustedTo (Size $ Vec 2 4) "AAA"
          [ [ "0-0", "1-0" ]
          , [ "0-1" ]
          , [ "0-2", "1-2", "2-2" ]
          ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "AAA" ]
              , [ "0-2", "1-2" ]
              , [ "AAA", "AAA" ]
              ]

    describe "fromArraysAdjusted" do
      it "exists" do
        G.fromArraysAdjusted ""
          [ [ "0-0", "1-0" ]
          , [ "0-1", "1-1" ]
          , [ "0-2", "1-2" ]
          ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

    describe "fromArray" do
      it "works for a simple example" do
        G.fromFlatArray (Size $ Vec 2 3) ""
          [ "0-0", "1-0", "0-1", "1-1", "0-2", "1-2" ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]

    describe "toArrays" do
      it "works for a simple example" do
        do
          G.toArrays $
            G.fromArraysAdjusted ""
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
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            G.fromArraysAdjusted ""
              [ [ "0-2", "0-1", "0-0" ]
              , [ "1-2", "1-1", "1-0" ]
              ]