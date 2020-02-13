{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-core"
, dependencies =
    [ "prelude"
    , "arrays"
    , "argonaut"
    , "foldable-traversable"
    , "integers"
    , "profunctor-lenses"
    , "tuples"
    , "effect"
    , "console"
    , "debug"
    , "psci-support"
    , "spec"
    ]
, packages =
    ../packages.dhall
}
