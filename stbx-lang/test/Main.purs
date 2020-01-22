module Test.Main where

import Prelude
import Effect.Aff (launchAff)
import Effect (Effect)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Language.Statebox.Wiring as Wiring
import Test.Language.Statebox.Net as Net

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  Wiring.spec
  Net.spec
