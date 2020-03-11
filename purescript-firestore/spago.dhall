{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-firestore"
, dependencies =
    [ "aff-promise"
    , "argonaut"
    , "bytestrings"
    , "foldable-traversable"
    , "foreign-object"
    , "functions"
    , "maybe"
    , "precise-datetime"
    , "psci-support"
    , "spec"
    , "strings"
    ]
, packages =
    ./../packages.dhall
}