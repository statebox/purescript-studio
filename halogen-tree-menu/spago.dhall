{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "halogen-tree-menu"
, dependencies =
    [ "halogen", "psci-support" ]
, packages =
    ./packages.dhall
}
