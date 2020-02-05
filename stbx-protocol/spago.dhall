{ sources =
    [ "src/**/*.purs" ]
, name =
    "stbx-protocol"
, dependencies =
    [ "stbx-core", "studio-common", "stbx-tx-store" ]
, packages =
    ./../packages.dhall
}
