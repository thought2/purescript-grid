{ name = "grid"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "spec"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "vectors"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
