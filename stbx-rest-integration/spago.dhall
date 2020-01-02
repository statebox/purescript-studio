{ sources =
    [ "test/**/*.purs" ]
, name =
    "stbx-rest-integration"
, dependencies =
    [ "stbx-client-rest"
    , "stbx-service-rest"
    ]
, packages =
    ./../packages.dhall
}