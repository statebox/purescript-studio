module Test.Main where

import Prelude
import Effect.Aff (Aff, launchAff)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.String (trim)
import Language.Statebox as Statebox
import Language.Statebox.Generator (mkParseResult) as Generator
import Language.Statebox.Generator.Net as Net
import Test.Spec                  (describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Debug.Trace (spy)

net1src :: String
net1src = trim """
login : guest -> loggedIn
buy : loggedIn -> loggedIn, cartNonEmpty
checkout : loggedIn, cartNonEmpty -> loggedOut
"""

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  describe "petri net parser" do
    it "should do stuff" do
      let ast = Statebox.parseNet net1src
      let astDump = spy "ast" $ show $ ast
      let pr1 = spy "pr1" $ Generator.mkParseResult <$> ast
      let net1 = spy "net1" $ Net.toNetWithDefaultName "dummy" <$> ast
      42 `shouldEqual` 42
