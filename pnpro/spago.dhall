{ name =
    "pnpro"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "group"
    , "halogen-petrinet-editor"
    , "ordered-collections"
    , "psci-support"
    , "studio-common"
    , "vec"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
