{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "stbx-store"
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