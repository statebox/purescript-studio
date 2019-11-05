{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-tx-store"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "free"
    , "psci-support"
    , "stbx-core"
    , "stbx-example-data"
    ]
, packages =
    ./../packages.dhall
}