{ name = "grid"
, dependencies =
  [ "console", "effect", "linear", "ordered-collections", "prelude", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
