module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))

main :: Effect Unit
main = run [consoleReporter] do
  describe "NLL Petri net encoding" do
    it "accept even length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0,4,5,5,0] `shouldEqual` Right [Tuple [1,2] [3], Tuple [3] [4,5,5]]
    it "reject odd length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0] `shouldEqual` Left ErrOddLength
    pending "infer a single trailing zero?"
