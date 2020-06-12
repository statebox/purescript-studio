{ name =
    "vec"
, dependencies =
    [ "console"
    , "effect"
    , "foldable-traversable"
    , "psci-support"
    , "spec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
