module Data.CharGrid
  ( CharGrid
  , fromString
  , fromStringAdjustedTo
  , toString
  ) where

import Prelude

import Data.Either (Either)
import Data.Grid (ErrorFromArrays, Grid, Size)
import Data.Grid as Grid
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex (replace) as Reg
import Data.String.Regex.Flags (noFlags) as Reg
import Data.String.Regex.Unsafe (unsafeRegex) as Reg

type CharGrid = Grid Char

toString :: CharGrid -> String
toString = Grid.toArrays >>> map fromCharArray >>> Str.joinWith "\n"

fromString :: String -> Either ErrorFromArrays CharGrid
fromString = toArray2d >>> Grid.fromArrays

fromStringAdjustedTo :: Size -> Char -> String -> CharGrid
fromStringAdjustedTo siz def = toArray2d
  >>> Grid.fromArraysAdjustedTo siz def

trimNewlines :: String -> String
trimNewlines =
  Reg.replace leadingNewlines "" >>> Reg.replace trailingNewlines ""
  where
  leadingNewlines = Reg.unsafeRegex "^\\n*" Reg.noFlags
  trailingNewlines = Reg.unsafeRegex "\\n*$" Reg.noFlags

toArray2d :: String -> Array (Array Char)
toArray2d = trimNewlines
  >>> Str.split (Str.Pattern "\n")
  >>> map toCharArray
