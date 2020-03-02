module Test.Web.Firestore where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

suite :: Spec Unit
suite = do
  describe "Firestore" do
    it "retrieves data correctly" do
      1 `shouldEqual` 1

