{ name =
    "vscode-studio"
, dependencies =
    [ "console", "effect", "psci-support", "stbx-lang", "studio" ]
, packages =
    ../packages.dhall
, sources =
    [ "src-ps/**/*.purs", "test-ps/**/*.purs" ]
}
