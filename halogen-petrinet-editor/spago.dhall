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
    , "stbx-core"
    , "studio-common"
    ]
, packages =
    ./packages.dhall
}
