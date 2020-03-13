{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-client-rest"
, dependencies =
    [ "affjax"
    , "aff-coroutines"
    , "argonaut"
    , "effect"
    , "free"
    , "console"
    , "psci-support"
    , "stbx-core"
    , "stbx-service-rest"
    ]
, packages =
    ../packages.dhall
}
