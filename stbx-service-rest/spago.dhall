{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-service-rest"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "express"
    , "psci-support"
    , "stbx-core"
    , "stbx-client-rest"
    , "stbx-tx-store"
    , "stbx-example-data"
    ]
, packages =
    ./../packages.dhall
}