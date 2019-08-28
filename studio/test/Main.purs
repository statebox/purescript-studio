module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Vec3 (vec3)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Diagram.FromNLL as Diagram

import Test.View.Petrinet.Model.NLL (suite) as NLL

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  NLL.suite
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
