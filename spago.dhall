{ name =
    "statebox-kdmoncat"
, dependencies =
    [ "argonaut-core"
    , "console"
    , "debug"
    , "effect"
    , "functors"
    , "halogen"
    , "halogen-svg"
    , "ordered-collections"
    , "profunctor-lenses"
    , "psci-support"
    , "strings"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
