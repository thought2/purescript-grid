module Test.Data.Grid where

import Prelude

import Data.Either (Either(..))
import Data.Grid (Pos(..), Size(..), vec2)
import Data.Grid as G
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Linear.Vec2 (getX, getY)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Grid" do
    describe "fill" do
      it "works for a simple example" do
        G.fill (Size $ vec2 2 3)
          (\(Pos vec) -> show (getX vec) <> "-" <> show (getY vec))
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

    describe "lookup" do
      it "works for a simple example" do
        do
          G.lookup (Pos $ vec2 1 2) $
            G.fromArraysAdjusted ""
              [ [ "0-0", "1-0" ]
              , [ "0-1", "1-1" ]
              , [ "0-2", "1-2" ]
              ]
          `shouldEqual`
            Just "1-2"

    describe "lookupModulo" do
      it "works for a simple example" do
        do
          G.lookupModulo (Pos $ vec2 2 3) $
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
            [ Pos $ vec2 0 0
            , Pos $ vec2 0 1
            , Pos $ vec2 0 2
            , Pos $ vec2 1 0
            , Pos $ vec2 1 1
            , Pos $ vec2 1 2
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
            [ (Pos $ vec2 0 0) /\ "0-0"
            , (Pos $ vec2 0 1) /\ "0-1"
            , (Pos $ vec2 0 2) /\ "0-2"
            , (Pos $ vec2 1 0) /\ "1-0"
            , (Pos $ vec2 1 1) /\ "1-1"
            , (Pos $ vec2 1 2) /\ "1-2"
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
            (Size $ vec2 2 3)

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
            Just $ (Pos $ vec2 1 1) /\ "1-1"

    describe "insert" do
      it "works for a simple example" do
        do
          G.insert (Pos $ vec2 1 1) "ABC" $
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

    describe "attemptInsert" do
      it "works for a simple example" do
        do
          G.attemptInsert (Pos $ vec2 1 1) "ABC" $
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

    describe "insertSubgrid" do
      it "works for a simple example" do
        G.insertSubgrid (Pos $ vec2 1 1)
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

    describe "insertSubgridCropped" do
      it "works for a simple example" do
        G.insertSubgridCropped (Pos $ vec2 2 1)
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
        G.fromArraysAdjustedTo (Size $ vec2 2 4) "AAA"
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
        G.fromFlatArray (Size $ vec2 2 3) ""
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