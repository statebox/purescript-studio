{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "halogen-petrinet-editor"
, dependencies =
    [ "console"
    , "effect"
    , "group"
    , "halogen-svg"
    , "psci-support"
    , "studio-common"
    ]
, packages =
    ../packages.dhall
}
