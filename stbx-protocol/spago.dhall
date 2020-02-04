{ sources =
    [ "src/**/*.purs" ]
, name =
    "stbx-protocol"
, dependencies =
    [ "halogen-petrinet-editor"
    , "stbx-core"
    , "stbx-tx-store"
    , "studio-common"
    ]
, packages =
    ./../packages.dhall
}
