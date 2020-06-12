{ name =
    "halogen-kdmoncat"
, dependencies =
    [ "argonaut-core"
    , "argonaut-codecs"
    , "console"
    , "debug"
    , "effect"
    , "functors"
    , "halogen"
    , "halogen-grid-kit"
    , "halogen-svg"
    , "kdmoncat"
    , "ordered-collections"
    , "profunctor-lenses"
    , "psci-support"
    , "random"
    , "strings"
    , "variant"
    , "vec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
