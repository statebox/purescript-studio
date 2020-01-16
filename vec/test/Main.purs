module Test.Main where

import Prelude
import Effect.Aff (launchAff)
import Effect (Effect)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Data.Vec3.AffineTransform as AffineTransform

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  AffineTransform.spec
