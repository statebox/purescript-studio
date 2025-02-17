module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))

import Test.Statebox.Core as Core
import Test.Statebox.Core.Execution as Execution
import Test.Statebox.Core.Marking as Marking
import Test.Statebox.Core.Transaction.Codec as Transaction.Codec
import Test.Statebox.Core.WiringTree as WiringTree

main :: Effect (Fiber Unit)
main = launchAff $ runSpec [consoleReporter] do
  describe "NLL Petri net encoding" do
    it "should accept even length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0,4,5,5,0] `shouldEqual` Right [Tuple [1,2] [3], Tuple [3] [4,5,5]]
    it "should reject odd length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0] `shouldEqual` Left ErrOddLength
    pending "should infer a single trailing zero?"

  Core.suite
  Execution.suite
  Marking.suite
  Transaction.Codec.suite
  WiringTree.suite
