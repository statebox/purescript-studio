{ name =
    "statebox-kdmoncat"
, dependencies =
    [ "argonaut-core"
    , "argonaut-codecs"
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
    , "vec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
