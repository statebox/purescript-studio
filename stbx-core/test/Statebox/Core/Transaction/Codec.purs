module Test.Statebox.Core.Transaction.Codec where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either.Nested (type (\/))
import Data.Either (Either(..), either)
import Data.NonEmpty (singleton) as NonEmpty
import Debug.Trace (spy)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Statebox.Core.Transaction (TxSum(..), Tx, FiringTx, evalTxSum)
import Statebox.Core.Types (Firing)
import Statebox.Core.Transaction.Codec (decodeTxTxSum, decodeFiringTx, decodeTxFiringTx)

suite :: Spec Unit
suite = do
  describe "Transaction codec" do
    it "should decode FiringTx without a 'message' field correctly" do
      let
        firingWithoutMessageFieldStr :: String
        firingWithoutMessageFieldStr =
          """{""" <>
          """  "firing": {""" <>
          """    "path": [4]""" <>
          """  },""" <>
          """  "previous": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5" """ <>
          """}"""

        firingWithoutMessageField :: String \/ (String \/ FiringTx)
        firingWithoutMessageField = spy "firingWithoutMessageField"  $ map decodeFiringTx $ decodeJson =<< jsonParser firingWithoutMessageFieldStr

      pure unit

    it "should decode a firing TxSum with a 'message' field correctly" do
      let
        txFiringTxWithMessageFieldStr :: String
        txFiringTxWithMessageFieldStr =
          """{""" <>
          """  "status": "200",""" <>
          """  "hex": "DEADBEEF",""" <>
          """  "hash": "DUMMY",""" <>
          """  "decoded": {""" <>
          """      "firing": {""" <>
          """        "execution": "a string",""" <>
          """        "message": "camels are awesome",""" <>
          """        "path": [4]""" <>
          """      },""" <>
          """      "previous": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5" """ <>
          """    }""" <>
          """}"""

        txFiringTxWithMessageField :: _ \/ (String \/ Tx TxSum)
        txFiringTxWithMessageField = spy "txFiringTxWithMessageFieldStr" $ map decodeTxTxSum $ decodeJson =<< jsonParser txFiringTxWithMessageFieldStr

      txFiringTxWithMessageField # either fail (either (show >>> fail) (_.decoded >>> evalTxSum
        (\ur -> fail "UberRootTxInj")
        (\r  -> fail "InitialTxInj")
        (\w  -> fail "WiringInj")
        (\f  -> f `shouldEqual`
            { "firing": {
                "execution": pure "a string",
                "message": pure "camels are awesome",
                "path": NonEmpty.singleton 4
              },
              "previous": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5"
            }
        )
        ))
