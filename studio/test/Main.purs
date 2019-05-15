module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Vec3 (vec3)
import Effect (Effect)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.Diagram.FromNLL as Diagram
import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.NLL (NetF, ErrNetEncoding(..))
import View.Petrinet.Model (PID, NetRep)
import View.Petrinet.Model.NLL as NLLToNet

import Test.Statebox.Core.Execution as Execution

main :: Effect Unit
main = run [consoleReporter] do

  describe "NLL Petri net encoding" do
    it "should accept even length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0,4,5,5,0] `shouldEqual` Right [Tuple [1,2] [3], Tuple [3] [4,5,5]]
    it "should reject odd length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0] `shouldEqual` Left ErrOddLength
    pending "should infer a single trailing zero?"

  describe "NetRep should be constructed correctly from" do
    it "an empty NLL-encoded net" do
      let netRep = NLLToNet.toNetRepWithDefaults mempty mempty mempty
      netRep.places `shouldEqual` []

    it "a simple NLL-encoded net" do
      let
        net1 = { name: "bar"
               , partition: [1,2,0,3,0,3,0,4,5,0]
               , names: ["t1", "t2"]
               , placeNames: ["p1", "p2", "p3", "p4", "p5"]
               }

        netNLLM :: Maybe (NetF PID)
        netNLLM = Net.fromNLLMaybe 0 net1.partition

        netRepM :: Maybe NetRep
        netRepM = (\net -> NLLToNet.toNetRepWithDefaults net net1.placeNames net1.names) <$> netNLLM

      ((Map.lookup 0 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` Nothing
      ((Map.lookup 1 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` pure "p1"
      ((Map.lookup 2 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` pure "p2"
      ((Map.lookup 3 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` pure "p3"
      ((Map.lookup 4 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` pure "p4"
      ((Map.lookup 5 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` pure "p5"
      ((Map.lookup 6 <<< _.placeLabelsDict) =<< netRepM) `shouldEqual` Nothing

      ((Map.lookup 5 <<< _.transitionLabelsDict) =<< netRepM) `shouldEqual` Nothing
      ((Map.lookup 6 <<< _.transitionLabelsDict) =<< netRepM) `shouldEqual` pure "t1"
      ((Map.lookup 7 <<< _.transitionLabelsDict) =<< netRepM) `shouldEqual` pure "t2"
      ((Map.lookup 8 <<< _.transitionLabelsDict) =<< netRepM) `shouldEqual` Nothing

  describe "Brick diagram encoding" do
    it "should decode a simple graph" do
       Diagram.fromNLL "test" [2, 2, 1, 8, 8, 8, 3] `shouldEqual`
           Right { name: "test"
                 , ops: [ { identifier: "0:0", pos: vec3 0 0 1, label: "2" }
                        , { identifier: "0:1", pos: vec3 0 1 1, label: "1" }
                        , { identifier: "1:0", pos: vec3 1 0 2, label: "8" }
                        , { identifier: "2:0", pos: vec3 2 0 1, label: "8" }
                        , { identifier: "2:1", pos: vec3 2 1 1, label: "3" }
                        ]
                 }

    it "should fail on cyclic graphs" do
      Diagram.fromNLL "test" [1, 1,2,3,1] `shouldEqual` Left Diagram.ErrGraphIsCyclic
--    it "should fail on multiple graphs with one cyclic" do
--       Diag.fromNLL [2,2,5,3,5,4,6,0,5] "test" `shouldEqual` Left Diag.ErrGraphIsCyclic

  Execution.suite
