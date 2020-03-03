module Statebox.Service.Error where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>), (~>?))
import Data.Either (Either(..), hush)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

import Statebox.Core (DecodeError(..)) as Stbx
import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)
import Statebox.Protocol (ProcessError(..))
import Statebox.Service.Status (Status(..), statusCode)

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
  TxNotFound       i -> "Transaction hash '" <> i.hash <> "' not found."
  TxNotHex         i -> "Transaction hash '" <> i.txHex <> "' is not a hexadecimal string."
  TxNoTxField        -> "You must pass a JSON body with a 'tx' attribute containing a hex-encoded Statebox transaction."
  TxDecodeFail     i -> "Failed to decode transaction with hash '" <> i.txHex <> "'."
  RootNonexistPrev i -> "Root transaction must have 'previous' attribute set to 'z', yours has '" <> i.previous <> "'."
  InitExecExists     -> "Execution already exists."
  InitNonexistPrev i -> "Failed to fire, 'previous' hash '" <> i.previous <> "' could not be found."
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
  show = txErrorCode

encodeTxErrorField :: TxError -> Maybe (String /\ Json)
encodeTxErrorField = case _ of
  TxNotFound       i -> Just ("hash"     := i.hash)
  TxNotHex         i -> Just ("txHex"    := i.txHex)
  TxNoTxField        -> Nothing
  TxDecodeFail     i -> Just ("txHex"    := i.txHex)
  RootNonexistPrev i -> Just ("previous" := i.previous)
  InitExecExists     -> Nothing
  InitNonexistPrev i -> Just ("previous" := i.previous)
  InvalidState       -> Nothing
  TxNotEnabled       -> Nothing

encodeTxError :: TxError -> Json
encodeTxError txError =
     "tag" := txErrorCode txError
  ~> encodeTxErrorField txError
  ~>? jsonEmptyObject

instance encodeJsonTxError :: EncodeJson TxError where
  encodeJson = encodeTxError

decodeTxErrorCase :: String -> (Object Json -> String \/ TxError) -> Json -> String \/ TxError
decodeTxErrorCase caseTag decoder = decodeJson >=> \x -> do
  tag <- x .: "tag"
  if tag /= caseTag
  then Left "invalid tag"
  else decoder x

decodeTxError :: Json -> String \/ TxError
decodeTxError json
  =   decodeTxNotFound       json
  <|> decodeTxNotHex         json
  <|> decodeTxNoTxField      json
  <|> decodeTxDecodeFail     json
  <|> decodeRootNonexistPrev json
  <|> decodeInitExecExists   json
  <|> decodeInitNonexistPrev json
  <|> decodeInvalidState     json
  <|> decodeTxNotEnabled     json
  where
    decodeTxNotFound :: Json -> String \/ TxError
    decodeTxNotFound = decodeTxErrorCase "tx-not-found"
      \x -> do
        hash <- x .: "hash"
        pure $ TxNotFound { hash: hash }

    decodeTxNotHex :: Json -> String \/ TxError
    decodeTxNotHex = decodeTxErrorCase "tx-not-hex"
      \x -> do
        txHex <- x .: "txHex"
        pure $ TxNotHex { txHex: txHex }

    decodeTxNoTxField :: Json -> String \/ TxError
    decodeTxNoTxField = decodeTxErrorCase "tx-no-tx"
      \_ -> pure TxNoTxField

    decodeTxDecodeFail :: Json -> String \/ TxError
    decodeTxDecodeFail = decodeTxErrorCase "tx-decode-fail"
      \x -> do
       txHex <- x .: "txHex"
       pure $ TxDecodeFail { txHex: txHex }

    decodeRootNonexistPrev :: Json -> String \/ TxError
    decodeRootNonexistPrev = decodeTxErrorCase "root-nonexist-prev"
      \x -> do
        previous <- x .: "previous"
        pure $ RootNonexistPrev { previous: previous }

    decodeInitExecExists :: Json -> String \/ TxError
    decodeInitExecExists = decodeTxErrorCase "init-exec-exists"
      \_ -> pure InitExecExists

    decodeInitNonexistPrev :: Json -> String \/ TxError
    decodeInitNonexistPrev = decodeTxErrorCase "init-nonexist-prev"
      \x -> do
        previous <- x .: "previous"
        pure $ RootNonexistPrev { previous: previous }

    decodeInvalidState :: Json -> String \/ TxError
    decodeInvalidState = decodeTxErrorCase "invalid-state"
      \_ -> pure InvalidState

    decodeTxNotEnabled :: Json -> String \/ TxError
    decodeTxNotEnabled = decodeTxErrorCase "tx-not-enabled"
      \_ -> pure TxNotEnabled

instance decodeJsonTxError :: DecodeJson TxError where
  decodeJson = decodeTxError

--------------------------------------------------------------------------------

processErrorToTxError :: ProcessError -> TxError
processErrorToTxError = case _ of
  NoUberRoot                                                        -> TxNoTxField -- TODO: wrong, not the correct error message!
  InitialPreviousShouldBeUberRoot                  txId             -> RootNonexistPrev {previous: txId}
  WiringPreviousShouldBeInitial                    txId             -> TxNoTxField -- TODO: wrong, not the correct error message!
  FiringInitialShouldBeCreatedOnlyOnce             txId             -> InitExecExists
  FiringInitialShouldHavePrevious                  txId             -> InitNonexistPrev {previous: txId}
  FiringInitialPreviousShouldBeWiring              txId             -> InitNonexistPrev {previous: txId} -- TODO: wrong, not the correct error message!
  FiringInitialTransitionShouldBeInitial           txId             -> InitNonexistPrev {previous: txId} -- TODO: wrong, not the correct error message!
  FiringNormalShouldHaveExistingExecution          txId executionId -> InvalidState -- TODO: wrong, not the correct error message!
  FiringNormalPreviousShouldMatchCurrentState      txId executionId -> InvalidState
  FiringNormalExecutionShouldPointToExistingWiring txId executionId -> InvalidState -- TODO: wrong, not the correct error message!
  FiringNormalExecutionWiringShouldBeAWiring       txId executionId -> InvalidState -- TODO: wrong, not the correct error message!
  FiringNormalTransitionShouldBeEnabled            txId executionId -> TxNotEnabled


--------------------------------------------------------------------------------

data ResponseError
  = FailedBodyToJson             { error :: String          , body     :: String }
  | FailedJsonToTxString         { error :: String          , jsonBody :: Json   }
  | FailedTxStringToTxJsonString { error :: Stbx.DecodeError, hash     :: HexStr }
  | FailedTxJsonStringToTxData   { error :: String          , hash     :: HexStr, txString :: String }
  | FailedTxDataToTxSum          { error :: String          , hash     :: HexStr, txData   :: Json   }

instance showResponseError :: Show ResponseError where
  show = case _ of
    FailedBodyToJson             o -> "The received body does not contain valid Json: \"" <> show o.body <> "\". The specific error is: " <> o.error
    FailedJsonToTxString         o -> "The received body does not contain JSON compliant with the specification: \"" <> stringify o.jsonBody <> "\". The specific error is: " <> o.error
    FailedTxStringToTxJsonString o -> "The received hash does not contain valid transaction data: \"" <> o.hash <> "\". The specific error is: " <> show o.error
    FailedTxJsonStringToTxData   o -> "The received transaction data do not contain JSON compliant with the js specification: \"" <> o.txString <> "\". The specific error is: " <> o.error
    FailedTxDataToTxSum          o -> "The received transaction data do not contain JSON compliant with the ps specification: \"" <> stringify o.txData <> "\". The specific error is: " <> o.error

responseErrorToTxError :: ResponseError -> TxError
responseErrorToTxError (FailedBodyToJson             o) = TxNoTxField -- this happens if the body of the request does not contain valid Json, not necessarily because there is no "tx" field
responseErrorToTxError (FailedJsonToTxString         o) = TxNoTxField -- this happens if the decoding of the body into a record containing a tx field, which should contain a string, fails, not necessarily because there is no "tx" field
responseErrorToTxError (FailedTxStringToTxJsonString o) = case o.error of
  Stbx.MissingRequiredField message -> TxDecodeFail { txHex : o.hash }
  Stbx.InvalidHexString             -> TxNotHex     { txHex : o.hash }
  Stbx.IndexOutOfRange      message -> TxDecodeFail { txHex : o.hash }
  Stbx.InvalidWireType      message -> TxDecodeFail { txHex : o.hash }
  Stbx.Other                message -> TxDecodeFail { txHex : o.hash }
responseErrorToTxError (FailedTxJsonStringToTxData   o) = TxDecodeFail { txHex : o.hash }
responseErrorToTxError (FailedTxDataToTxSum          o) = TxDecodeFail { txHex : o.hash }

decodeResponseError :: Json -> String \/ ResponseError
decodeResponseError json
  =   FailedBodyToJson             <$> decodeFailedBodyToJson json
  <|> FailedJsonToTxString         <$> decodeFailedJsonToTxString json
  <|> FailedTxStringToTxJsonString <$> decodeFailedTxStringToTxJsonString json
  <|> FailedTxJsonStringToTxData   <$> decodeFailedTxJsonStringToTxData json
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

decodeFailedTxJsonStringToTxData :: Json -> String \/ { hash :: HexStr, txString :: String, error :: String }
decodeFailedTxJsonStringToTxData = decodeJson >=> \x -> do
  hash     <- x .: "hash"
  txString <- x .: "txString"
  error    <- x .: "error"
  pure { hash : hash, txString : txString, error : error }

decodeFailedTxDataToTxSum :: Json -> String \/ { hash :: HexStr, txData :: Json, error :: String }
decodeFailedTxDataToTxSum = decodeJson >=> \x -> do
  hash   <- x .: "hash"
  txData <- x .: "txData"
  error  <- x .: "error"
  pure { hash : hash, txData : txData, error : error }

instance decodeJsonResponseError :: DecodeJson ResponseError where
  decodeJson = decodeResponseError

--------------------------------------------------------------------------------

-- TODO add "data" field modeled after the `StateboxException` code in the js codebase
type TxErrorResponseBody =
  { status  :: String
  , code    :: String
  , message :: String
  , error   :: Json
  }

-- TODO: these two function form a Prism, so we make this explicit in the types?
toTxErrorResponseBody :: TxError -> TxErrorResponseBody
toTxErrorResponseBody err =
  { status  : statusCode Failed
  , code    : txErrorCode err
  , message : txErrorMessage err
  , error   : encodeTxError err
  }

fromTxErrorResponseBody :: TxErrorResponseBody -> Maybe TxError
fromTxErrorResponseBody responseBody = hush $ decodeJson responseBody.error
