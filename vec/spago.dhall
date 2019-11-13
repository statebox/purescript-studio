{ name =
    "vec"
, dependencies =
    [ "console"
    , "effect"
    , "foldable-traversable"
    , "psci-support"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
