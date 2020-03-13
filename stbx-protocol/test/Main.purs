module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Statebox.Protocol.Fire as Fire

main :: Effect (Fiber Unit)
main = launchAff $ runSpec [consoleReporter] do
  Fire.suite
