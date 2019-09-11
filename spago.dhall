{ name =
    "statebox-kdmoncat"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "functors"
    , "halogen"
    , "halogen-svg"
    , "ordered-collections"
    , "profunctor-lenses"
    , "psci-support"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
