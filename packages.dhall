let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221214/packages.dhall
        sha256:e462fb4d932e4bbc522cb563a71d312d6514f97050125d1a3f95cc3a2df3bffb

in  upstream
  with vectors =
      { dependencies =
      [ "console"
      , "effect"
      , "foldable-traversable"
      , "prelude"
      , "profunctor-lenses"
      ]
      , repo =
          "https://github.com/thought2/purescript-vectors.git"
      , version =
          "v2.0.1"
      }