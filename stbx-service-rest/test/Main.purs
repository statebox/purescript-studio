module Test.Main where

import Prelude

import Affjax (URL)
import Affjax.ResponseFormat (ResponseFormatError(..))
import Data.Either (Either(..))
import Debug.Trace (spy)
import Effect.Aff (Fiber, launchAff)
import Effect (Effect)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

import Statebox.Core.Transaction (TxId)
import Statebox.Core.Transaction.Codec (DecodingError(..))
import Statebox.Client as Stbx
import Statebox.Client (evalTransactionResponse)

import Test.Common

endpointUrl :: URL
endpointUrl = "http://127.0.0.1:8080"

main :: Effect (Fiber Unit)
main = launchAff $ runSpec [consoleReporter] do

  -- first we post our example transactions to the API
  postExampleTransactionsSpec

  -- then we try to read them back in
  getExampleTransactionsSpec


postExampleTransactionsSpec :: Spec Unit
postExampleTransactionsSpec =
  describe "Statebox transaction API HTTP service" do
    it "should respond to POST <root tx> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a0022200a1e47756172616e746565642d456e7472616e63652d546f6b656e2e74657374"
      let dump1 = spy "POST response: " res
      todo
    it "should respond to POST <wiring tx> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a20dce4021c8f117e89c479665f6d61ff650b150af375d6498b593da6afa8d2ca9f1afa010add010a0a70726976696c656467651001100010021000100210001006100010011000100310001003100010011000100210001004100010031000100510001004100010051000100110001005100010021000100510001006100010021000100610001003100010061000100510001000100310001a036275791a07666f7253616c651a05626c6f636b1a07756e626c6f636b1a047363616e1a086e6f74536f6c64321a0873686f774f7665721a076e6f74536f6c641a066e6f53686f771a04627579271a076275794261636b1a096e6f745265736f6c641a0663726561746512160a046d61696e10011801220a70726976696c656467651800"
      let dump1 = spy "POST response: " res
      todo
    it "should respond to POST <0th firing tx (the 'execution')> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a20dce4021cacc5f351d54402799977d7e4f7b86805359aec724805c80ec0b4d546120710001a03aa0003"
      let dump1 = spy "POST response: " res
      todo
    it "should respond to POST <1st firing tx> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b812240a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b81004"
      let dump1 = spy "POST response: " res
      todo
    it "should respond to POST <2nd firing tx> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a20dce4021cd649d3a9d1f69832f26739c1d81c873ca5f343ec2dd92d335adfc805122a0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810011a04111aaa11"
      let dump1 = spy "POST response: " res
      todo
    it "should respond to POST <3rd firing tx> correctly" do
      res <- Stbx.postTransactionHex endpointUrl "0a20dce4021c15fda2dfd9ec2ef4413b9e5a4ac5cbd8def33c0ca2c071f75a71464b122b0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810051a05222bbbb222"
      let dump1 = spy "POST response: " res
      todo

getExampleTransactionsSpec :: Spec Unit
getExampleTransactionsSpec =
  describe "Statebox transaction API HTTP service" do
    pending "TODO: GETting root transaction fails"
    -- requestTransactionSpec "root"                 "zFsGM27VMNWZne1SSkWnDQTzr6TdjmsKpbxGkJKKaEC8e"
    requestTransactionSpec "wiring"               "zFsGM27o59f9Lu8bWjNHBG7Wbq5iftQA6uDt14zRdjCrH"
    requestTransactionSpec "firing 0 (execution)" "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5"
    requestTransactionSpec "firing 1"             "zFsGM28DqZKjjGbfCEsjsXTj8xJAqWaBXpDSc1CqR6ihi"
    requestTransactionSpec "firing 2"             "zFsGM26F88jGH8HtpdSCBdgRWSVJEWbyDoH1HRRWXTZyC"
    requestTransactionSpec "firing 3"             "zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV"

--------------------------------------------------------------------------------

requestTransactionSpec :: String -> TxId -> Spec Unit
requestTransactionSpec txDescription requestedHash =
  it ("should respond to GET /tx/" <> requestedHash <> " for " <> txDescription <> " correctly") do
    res <- Stbx.requestTransaction endpointUrl requestedHash
    res # evalTransactionResponse
      (\(ResponseFormatError e obj) -> fail $ "ResponseFormatError: " <> show e) -- TODO spy obj?
      (\(DecodingError e)           -> fail $ "DecodingError: " <> e)
      (\{id, tx}                    -> succeed)                                  -- TODO more checks

-- TODO for now
todo = succeed
