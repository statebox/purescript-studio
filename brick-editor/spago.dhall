{ name =
    "brick-editor"
, dependencies =
    [ "vec"
    , "halogen-svg"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
