{ name = "grid"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "linear"
  , "maybe"
  , "naturals"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "spec"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
