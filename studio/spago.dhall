{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "statebox-studio"
, dependencies =
    [ "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "debug"
    , "effect"
    , "halogen"
    , "halogen-diagram-editor"
    , "halogen-kdmoncat"
    , "halogen-petrinet-editor"
    , "halogen-tree-menu"
    , "pnpro"
    , "prelude"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "stbx-core"
    , "stbx-lang"
    , "stbx-client-rest"
    , "spec"
    , "strings"
    , "studio-common"
    , "web-uievents"
    , "vec"
    ]
, packages =
    ../packages.dhall
}
