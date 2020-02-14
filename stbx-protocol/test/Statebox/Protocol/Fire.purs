module Test.Statebox.Protocol.Fire where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

suite :: Spec Unit
suite = do
  describe "Firing a firing" do
    it "should work" do
      1 `shouldEqual` 1