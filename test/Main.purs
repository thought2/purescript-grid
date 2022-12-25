module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Data.Grid as T.Data.Grid

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  T.Data.Grid.spec
