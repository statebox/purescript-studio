module Test.Statebox.Core.Transition where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.ArrayMultiset (ArrayMultiset)
import Statebox.Core.Transition (buildPlaceMarkings)

suite :: Spec Unit
suite = do
  describe "Transition conversion from multiset to array of place markings" do
    it "produces an empty array from an empty multiset" do
      buildPlaceMarkings ([] :: ArrayMultiset Int) `shouldEqual` []
    it "produces a singleton from a singleton" do
      buildPlaceMarkings [1] `shouldEqual` [{ place: 1, tokens: 1 } ]
    it "counts correctly the number of tokens in a place" do
      buildPlaceMarkings [1, 1, 1] `shouldEqual` [{ place: 1, tokens: 3 }]
    it "handles correctly multiple places" do
      buildPlaceMarkings [1, 1, 1, 2, 3, 3] `shouldEqual` [ { place: 1, tokens: 3}
                                                          , { place: 2, tokens: 1}
                                                          , { place: 3, tokens: 2}
                                                          ]
