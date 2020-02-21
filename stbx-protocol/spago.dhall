{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-protocol"
, dependencies =
    [ "stbx-core", "studio-common", "stbx-store" ]
, packages =
    ./../packages.dhall
}
