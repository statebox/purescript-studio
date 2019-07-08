let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190427/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190427/src/packages.dhall sha256:6b17811247e1f825034fa4dacc4b8ec5eddd0e832e0e1579c2ba3b9b2a1c63fe

let overrides = {=}

let additions =
  -- ((../halogen-petrinet-editor/packages.dhall).dependencies) //
  -- (../halogen-petrinet-editor/packages.dhall) //
  { halogen-petrinet-editor =
      mkPackage
        (../halogen-petrinet-editor/spago.dhall).dependencies
        "../halogen-petrinet-editor"
        "development"

  , halogen-svg =
      mkPackage
        [ "prelude"
		, "halogen"
		, "strings"
		, "web-uievents"
		, "effect"
		]
        "https://github.com/statebox/purescript-halogen-svg.git"
        "master"
  , studio-common =
      mkPackage
        [ "prelude" ]
        "../studio-common"
        "development"
  }

in  upstream // overrides // additions
