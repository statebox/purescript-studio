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
    , "group"
    , "halogen"
    , "halogen-svg"
    , "halogen-petrinet-editor"
    , "halogen-tree-menu"
    , "pnpro"
    , "prelude"
    , "psci-support"
    , "stbx-core"
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
