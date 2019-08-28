let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190427/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides = 
      { halogen-petrinet-editor = ../halogen-petrinet-editor/spago.dhall as Location }

let additions = ../halogen-petrinet-editor/packages.dhall

in  upstream // overrides // additions
