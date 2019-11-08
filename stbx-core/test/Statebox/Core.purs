module Test.Statebox.Core where

import Prelude
import Effect.Class (liftEffect)
import Effect.Console (log)

import Statebox.Core as Stbx

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Debug.Trace (spy)

-- | We define this FFI value in order to load the FFI module, which imports (requires) stbx.js.
foreign import requireStbxJs_HACK :: String

suite :: Spec Unit
suite = do
  describe "Stbx" do
    it "should decode a wiring transaction from hex correctly" do
      decodedStbxObj <- liftEffect $ Stbx.decode "0a04deadbeef1a2c0a150a01611000100110001001100010001a01781a0179120f0a017a10011801180222017322017418001800"
      let decodedJsonString = Stbx.stbxObjToJsonString decodedStbxObj
      decodedJsonString `shouldEqual` """{"wiring":{"nets":[{"name":"a","partition":[0,1,0,1,0,0],"names":["x","y"]}],"diagrams":[{"name":"z","width":1,"pixels":[1,2],"names":["s","t"]}],"labels":[0,0]},"previous":"z6h8cQN"}"""
