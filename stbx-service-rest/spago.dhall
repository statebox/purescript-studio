{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-service-rest"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "express"
    , "free"
    , "psci-support"
    , "stbx-core"
    , "stbx-client-rest"
    ]
, packages =
    ./packages.dhall
}
