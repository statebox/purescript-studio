module Test.Main where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.NLL (NetF, ErrNetEncoding(..))
import View.Petrinet.Model (PID, NetRep)
import View.Petrinet.Model.NLL as NLLToNet

main :: Effect Unit
main = run [consoleReporter] do

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
