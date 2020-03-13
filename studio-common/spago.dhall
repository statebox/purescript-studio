{ name =
    "studio-common"
, dependencies =
    [ "console"
    , "effect"
    , "foreign-object"
    , "group"
    , "integers"
    , "ordered-collections"
    , "psci-support"
    , "stbx-core"
    , "vec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
