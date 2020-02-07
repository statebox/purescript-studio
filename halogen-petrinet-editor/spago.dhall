{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "halogen-petrinet-editor"
, dependencies =
    [ "console"
    , "effect"
    , "halogen-svg"
    , "psci-support"
    , "studio-common"
    , "vec"
    ]
, packages =
    ../packages.dhall
}
