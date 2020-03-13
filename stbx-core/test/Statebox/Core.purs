module Test.Statebox.Core where

import Prelude
import Data.Either (Either(..))

import Statebox.Core as Stbx

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | We define this FFI value in order to load the FFI module, which imports (requires) stbx.js.
foreign import requireStbxJs_HACK :: String

suite :: Spec Unit
suite = do
  describe "Stbx" do
    it "should decode a root transaction from hex correctly" do
      let eitherDecodedString = Stbx.decodeToJsonString "0a0022200a1e47756172616e746565642d456e7472616e63652d546f6b656e2e74657374"
      eitherDecodedString `shouldEqual` Right "{\"root\":{\"message\":\"47756172616e746565642d456e7472616e63652d546f6b656e2e74657374\"}}"

    it "should decode a wiring transaction from hex correctly" do
      let eitherDecodedString = Stbx.decodeToJsonString "0a04deadbeef1a2c0a150a01611000100110001001100010001a01781a0179120f0a017a10011801180222017322017418001800"
      eitherDecodedString `shouldEqual` Right """{"wiring":{"nets":[{"name":"a","partition":[0,1,0,1,0,0],"names":["x","y"]}],"diagrams":[{"name":"z","width":1,"pixels":[1,2],"names":["s","t"]}],"labels":[0,0]},"previous":"z6h8cQN"}"""

    it "should compute a hash from a hex string correctly" do
      let hash = Stbx.hash "0a04deadbeef1a2c0a150a01611000100110001001100010001a01781a0179120f0a017a10011801180222017322017418001800"
      hash `shouldEqual` "zFsGM26m8BF1jPQssCAq1dtXvksHGd5BgNp8GE7QXdwXF"
