module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))
import Data.Petrinet.Representation.NLL as Net
import Data.Tuple (Tuple(..))
import Data.Vec2D
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import View.Diagram.FromNLL as Diag
import Web.TouchEvent.Touch (identifier)

main :: Effect Unit
main = run [consoleReporter] do
  describe "NLL Petri net encoding" do
    it "accept even length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0,4,5,5,0] `shouldEqual` Right [Tuple [1,2] [3], Tuple [3] [4,5,5]]
    it "reject odd length encodings" do
      Net.fromNLL 0 [1,2,0,3,0,3,0] `shouldEqual` Left ErrOddLength
    pending "infer a single trailing zero?"
  describe "Brick diagram encoding" do
    it "decodes a simple graph" do
       Diag.fromNLL [2, 2, 1, 8, 8, 8, 3] "test" `shouldEqual`
           Right { name: "test"
                 , ops: [ { identifier: "0:0", pos: vec3 0 0 1, label: "2" }
                        , { identifier: "0:1", pos: vec3 0 1 1, label: "1" }
                        , { identifier: "1:0", pos: vec3 1 0 2, label: "8" }
                        , { identifier: "2:0", pos: vec3 2 0 1, label: "8" }
                        , { identifier: "2:1", pos: vec3 2 1 1, label: "3" }
                        ]
                 }

    it "should fail on cyclic graphs" do
       Diag.fromNLL [1, 1,2,3,1] "test" `shouldEqual` Left Diag.ErrGraphIsCyclic
--    it "should fail on multiple graphs with one cyclic" do
--       Diag.fromNLL [2,2,5,3,5,4,6,0,5] "test" `shouldEqual` Left Diag.ErrGraphIsCyclic
