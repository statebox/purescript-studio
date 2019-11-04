module Test.Main where

import Prelude
import Effect.Aff (Aff, launchAff)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec                  (describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Language.Statebox.Wiring as Wiring
import Test.Language.Statebox.Net as Net

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  Wiring.spec
  Net.spec
