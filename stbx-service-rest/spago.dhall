{ sources =
    [ "src/**/*.purs" ]
, name =
    "stbx-service-rest"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "express"
    , "psci-support"
    , "stbx-core"
    , "stbx-tx-store"
    ]
, packages =
    ./../packages.dhall
}