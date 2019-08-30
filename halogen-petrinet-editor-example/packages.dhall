let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides = 
      { halogen-petrinet-editor = ../halogen-petrinet-editor/spago.dhall as Location }

let additions = ../halogen-petrinet-editor/packages.dhall

in  upstream // overrides // additions
