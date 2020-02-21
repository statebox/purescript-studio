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
    , "stbx-store"
    ]
, packages =
    ./../packages.dhall
}