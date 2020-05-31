{ name =
    "stbx-cloud-console"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "console"
    , "debug"
    , "effect"
    , "halogen"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "studio"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
