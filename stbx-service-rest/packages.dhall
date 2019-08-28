let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides = {=}

let additions =
  { stbx-core = ../stbx-core/spago.dhall as Location
  , stbx-client-rest =../stbx-client-rest/spago.dhall as Location
  , stbx-example-data = ../stbx-example-data/spago.dhall as Location
  , express =
      mkPackage
        [ "aff"
        , "effect"
        , "console"
        , "foreign"
        , "foreign-generic"
        , "node-http"
        , "psci-support"
        , "test-unit"
        ]
        "https://github.com/nkly/purescript-express.git"
        "v0.8.0"
  }

in  upstream // overrides // additions
