let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides =
      { stbx-core = ../stbx-core/spago.dhall as Location
      , stbx-client-rest = ../stbx-client-rest/spago.dhall as Location
      , studio-common = ../studio-common/spago.dhall as Location
      , halogen-petrinet-editor = ../halogen-petrinet-editor/spago.dhall as Location
      , halogen-tree-menu = ../halogen-tree-menu/spago.dhall as Location
      , pnpro = ../pnpro/spago.dhall as Location
      }

let additions =
      { halogen-svg =
          { dependencies = [ "prelude", "halogen", "strings", "web-uievents", "effect" ]
          , repo = "https://github.com/statebox/purescript-halogen-svg.git"
          , version = "d0a4cbc79b5513296cb746576824dce967aedbab"
          }
      }

in  upstream // overrides // additions
