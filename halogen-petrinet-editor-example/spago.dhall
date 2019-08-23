{ name =
    "halogen-petrinet-editor-example"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
    , "halogen-petrinet-editor"
    , "prelude"
    , "psci-support"
    , "record"
    , "studio-common"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
