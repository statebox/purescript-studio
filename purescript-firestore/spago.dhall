{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-firestore"
, dependencies =
    [ "aff-promise"
    , "argonaut"
    , "bytestrings"
    , "foreign-object"
    , "functions"
    , "maybe"
    , "precise-datetime"
    , "psci-support"
    , "spec"
    ]
, packages =
    ./../packages.dhall
}
