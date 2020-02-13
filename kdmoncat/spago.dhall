{ name =
    "kdmoncat"
, dependencies =
    [ "argonaut-core"
    , "argonaut-codecs"
    , "psci-support"
    , "vec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
