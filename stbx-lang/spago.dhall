{ name =
    "stbx-lang"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "halogen-petrinet-editor"
    , "memoize"
    , "parsing"
    , "psci-support"
    , "spec"
    , "stbx-core"
    , "studio-common"
    , "vec"
    ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
