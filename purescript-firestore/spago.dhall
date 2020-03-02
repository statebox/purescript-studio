{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-firestore"
, dependencies =
    [ "aff-promise"
    , "bytestrings"
    , "functions"
    , "maybe"
    , "precise-datetime"
    , "spec"
    ]
, packages =
    ./../packages.dhall
}
