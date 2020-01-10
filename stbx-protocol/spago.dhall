{ sources =
    [ "src/**/*.purs" ]
, name =
    "stbx-protocol"
, dependencies =
    [ "stbx-core"
    , "stbx-tx-store"
    ]
, packages =
    ./../packages.dhall
}