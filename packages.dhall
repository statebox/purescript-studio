let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides = {=}

let additions = {
  halogen-svg =
    { dependencies = [ "prelude", "halogen", "strings", "web-uievents", "effect" ]
    , repo = "https://github.com/statebox/purescript-halogen-svg.git"
    , version = "d0a4cbc79b5513296cb746576824dce967aedbab"
    },
  vec =
    { dependencies = [ "foldable-traversable" ]
    , repo = "https://github.com/statebox/purescript-vec.git"
    , version = "c7c8486a4e36ed37baf67cbe026d42acd4aa9b02"
    }
}

in  upstream // overrides // additions
