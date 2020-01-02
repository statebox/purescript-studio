module Statebox.Service where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either.Nested (type (\/))
import Statebox.Core (DecodeError(..)) as Stbx
import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)

-- | Based on the `StateboxException`s thrown in https://github.com/statebox/cloud/blob/73158c3a779cbc8a6348aac60e2d0b21e907b2c1/services/tx/process-tx.js.
data TxError
  -- | TODO Align with js implementation.
  = TxNotFound { hash :: HashStr }

  -- | Additional error data to store: none.
  | TxNotHex { txHex :: HexStr }

  -- | Additional error data to store: none.
  | TxNoTxField

  -- | Additional error data to store: `{error: e}`.
  | TxDecodeFail { txHex :: HexStr }

  -- | Additional error data to store: none.
  | RootNonexistPrev { previous :: HashStr }

  -- | Additional error data to store: `{stateHash, executionId, enabled}`.
  | InitExecExists

  -- | Additional error data to store: none.
  | InitNonexistPrev { previous :: HashStr }

  -- | Additional error data to store: `{previous, stateHash}`.
  | InvalidState

  -- | Additional error data to store: `{enabled: en0, transition: tr}`.
  | TxNotEnabled

txErrorMessage :: TxError -> String
txErrorMessage = case _ of
  TxNotFound       i -> "Transaction " <> i.hash <> " not found."
  TxNotHex         i -> "Transaction " <> i.txHex <> " is not a hexadecimal string."
  TxNoTxField        -> "You must pass a JSON body with a 'tx' attribute containing a hex-encoded Statebox transaction."
  TxDecodeFail     i -> "Failed to decode transaction " <> i.txHex <> "."
  RootNonexistPrev i -> "Root transaction must have 'previous' attribute set to 'z', yours has " <> i.previous <> "."
  InitExecExists     -> "Execution already exists."
  InitNonexistPrev i -> "Failed to fire, 'previous' hash " <> i.previous <> " could not be found."
  InvalidState       -> "Invalid state."
  TxNotEnabled       -> "Transition not enabled."

txErrorCode :: TxError -> String
txErrorCode = case _ of
  TxNotFound       _ -> "tx-not-found" -- TODO confirm
  TxNotHex         _ -> "tx-not-hex"
  TxNoTxField        -> "tx-no-tx"
  TxDecodeFail     _ -> "tx-decode-fail"
  RootNonexistPrev _ -> "root-nonexist-prev"
  InitExecExists     -> "init-exec-exists"
  InitNonexistPrev _ -> "init-nonexist-prev"
  InvalidState       -> "invalid-state"
  TxNotEnabled       -> "tx-not-enabled"

instance showTxError :: Show TxError where
  show = show <<< txErrorCode

--------------------------------------------------------------------------------

data ResponseError
  = FailedBodyToJson             { body :: String, error :: String }
  | FailedJsonToTxString         { jsonBody :: Json, error :: String }
  | FailedTxStringToTxJsonString { hash :: HexStr, error :: Stbx.DecodeError }
  | FailedTxJsonToTxData         { txString :: String, error :: String }
  | FailedTxDataToTxSum          { txData :: Json, error :: String }

instance showResponseError :: Show ResponseError where
  show (FailedBodyToJson             o) = "The received body does not contain valid Json: \"" <> show o.body <> "\". The specific error is: " <> o.error
  show (FailedJsonToTxString         o) = "The received body does not contain Json compliant with the specification: \"" <> stringify o.jsonBody <> "\". The specific error is: " <> o.error
  show (FailedTxStringToTxJsonString o) = "The received hash does not contain valid transaction data: \"" <> o.hash <> "\". The specific error is: " <> show o.error
  show (FailedTxJsonToTxData         o) = "The received transaction data do not contain Json compliant with the js specification: \"" <> o.txString <> "\". The specific error is: " <> o.error
  show (FailedTxDataToTxSum          o) = "The received transaction data do not contain Json compliant with the ps specification: \"" <> stringify o.txData <> "\". The specific error is: " <> o.error

responseErrorToTxError :: ResponseError -> TxError
responseErrorToTxError (FailedBodyToJson             o) = TxNoTxField -- TODO: handle this case correctly
responseErrorToTxError (FailedJsonToTxString         o) = TxNoTxField -- this could actually fail also for other reasons
responseErrorToTxError (FailedTxStringToTxJsonString o) = case o.error of
  Stbx.MissingRequiredField message -> TxDecodeFail { txHex : o.hash }
  Stbx.InvalidHexString             -> TxNotHex { txHex : o.hash }
  Stbx.IndexOutOfRange      message -> TxDecodeFail { txHex : o.hash }
  Stbx.InvalidWireType      message -> TxDecodeFail { txHex : o.hash }
  Stbx.Other                message -> TxDecodeFail { txHex : o.hash }
responseErrorToTxError (FailedTxJsonToTxData         o) = TxNoTxField -- TODO: handle this case correctly
responseErrorToTxError (FailedTxDataToTxSum          o) = TxNoTxField -- TODO: handle this case correctly

decodeResponseError :: Json -> String \/ ResponseError
decodeResponseError json
  =   FailedBodyToJson             <$> decodeFailedBodyToJson json
  <|> FailedJsonToTxString         <$> decodeFailedJsonToTxString json
  <|> FailedTxStringToTxJsonString <$> decodeFailedTxStringToTxJsonString json
  <|> FailedTxJsonToTxData         <$> decodeFailedTxJsonToTxData json
  <|> FailedTxDataToTxSum          <$> decodeFailedTxDataToTxSum json

decodeFailedBodyToJson :: Json -> String \/ { body :: String, error :: String }
decodeFailedBodyToJson = decodeJson >=> \x -> do
  body  <- x .: "body"
  error <- x .: "error"
  pure { body : body, error : error }

decodeFailedJsonToTxString :: Json -> String \/ { jsonBody :: Json, error :: String }
decodeFailedJsonToTxString = decodeJson >=> \x -> do
  body  <- x .: "jsonBody"
  error <- x .: "error"
  pure { jsonBody : body, error : error }

decodeFailedTxStringToTxJsonString :: Json -> String \/ { hash :: HexStr, error :: Stbx.DecodeError }
decodeFailedTxStringToTxJsonString = decodeJson >=> \x -> do
  hash  <- x .: "hash"
  error <- x .: "error"
  pure { hash : hash, error : error }

decodeFailedTxJsonToTxData :: Json -> String \/ { txString :: String, error :: String }
decodeFailedTxJsonToTxData = decodeJson >=> \x -> do
  txString <- x .: "txString"
  error    <- x .: "error"
  pure { txString : txString, error : error }

decodeFailedTxDataToTxSum :: Json -> String \/ { txData :: Json, error :: String }
decodeFailedTxDataToTxSum = decodeJson >=> \x -> do
  txData <- x .: "txData"
  error  <- x .: "error"
  pure { txData : txData, error : error }

instance decodeJsonResponseError :: DecodeJson ResponseError where
  decodeJson = decodeResponseError

--------------------------------------------------------------------------------

-- TODO add "data" field modeled after the `StateboxException` code in the js codebase
type TxErrorResponseBody =
  { status  :: String
  , code    :: String
  , message :: String
  }

toTxErrorResponseBody :: TxError -> TxErrorResponseBody
toTxErrorResponseBody err =
  { status  : statusCode Failed
  , code    : txErrorCode err
  , message : txErrorMessage err
  }

--------------------------------------------------------------------------------

-- | TODO this is now used ad hoc in JSON responses; these should be made to conform to the Statebox protocol spec.
data Status = Ok | Failed

statusCode :: Status -> String
statusCode = case _ of
  Ok     -> "ok"
  Failed -> "failed"


instance showStatus :: Show Status where
  show = statusCode

statusToString :: Status -> String
statusToString = show -- TODO this should be a JSON-compatible value; perhaps a regular JSON encoder
