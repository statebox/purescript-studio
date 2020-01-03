module Statebox.Service.Codec where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Profunctor.Strong ((&&&))
import Data.Tuple.Nested (type (/\), (/\))

import Statebox.Core (decodeToJsonString) as Stbx
import Statebox.Core.Transaction (TxSum)
import Statebox.Core.Transaction.Codec (decodeTxSum)
import Statebox.Core.Types (HexStr)
import Statebox.Service (ResponseError(..), TxErrorResponseBody)


decodeTxErrorResponseBody :: Json -> String \/ TxErrorResponseBody
decodeTxErrorResponseBody = decodeJson >=> \x -> do
  status  <- x .: "status"
  code    <- x .: "code"
  message <- x .: "message"
  error   <- x .: "error"
  pure { status, code, message, error }

--------------------------------------------------------------------------------

type TxPostRequestBody = { tx :: HexStr }

parseBodyToJson :: String -> Either ResponseError Json
parseBodyToJson bodyStr = lmap
  (\error -> FailedBodyToJson { body : bodyStr, error : error })
  (jsonParser bodyStr)

jsonBodyToTxString :: Json -> Either ResponseError HexStr
jsonBodyToTxString jsonBody = bimap
  (\error -> FailedJsonToTxString { jsonBody : jsonBody, error : error })
  (\body -> body.tx)
  (decodeJson jsonBody :: String \/ TxPostRequestBody)

txStringToTxJsonString :: HexStr -> Either ResponseError String
txStringToTxJsonString txHex = lmap
  (\error -> FailedTxStringToTxJsonString { hash : txHex, error : error })
  (Stbx.decodeToJsonString txHex)

txStringToTxJsonString' :: HexStr -> Either ResponseError (HexStr /\ String)
txStringToTxJsonString' txHex =
  (const txHex &&& identity) <$> txStringToTxJsonString txHex

txJsonStringToTxData :: String -> Either ResponseError Json
txJsonStringToTxData decodedJsonString = lmap
  (\error -> FailedTxJsonToTxData { txString : decodedJsonString, error : error })
  (jsonParser decodedJsonString)

txJsonStringToTxData' :: (HexStr /\ String) -> Either ResponseError (HexStr /\ Json)
txJsonStringToTxData' (txHex /\ decodedJsonString) =
  (const txHex &&& identity) <$> (txJsonStringToTxData decodedJsonString)

txDataToTxSum :: Json -> Either ResponseError TxSum
txDataToTxSum txJson = lmap
  (\error -> FailedTxDataToTxSum {txData : txJson, error : error })
  (decodeTxSum txJson)

txDataToTxSum' :: (HexStr /\ Json) -> Either ResponseError (HexStr /\ TxSum)
txDataToTxSum' (txHex /\ txJson) =
  (const txHex &&& identity) <$> (txDataToTxSum txJson)
