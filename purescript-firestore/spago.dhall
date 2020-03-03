{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-firestore"
, dependencies =
    [ "aff-promise"
    , "bytestrings"
    , "foreign-object"
    , "functions"
    , "maybe"
    , "precise-datetime"
    , "spec"
    ]
, packages =
    ./../packages.dhall
}
