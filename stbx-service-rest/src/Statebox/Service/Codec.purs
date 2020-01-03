module Statebox.Service.Codec where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Traversable (traverse)
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
  pure { status, code, message }

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

txStringToTxJsonString :: HexStr -> Either ResponseError (HexStr /\ String)
txStringToTxJsonString txHex = bimap
  (\error -> FailedTxStringToTxJsonString { hash : txHex, error : error })
  (txHex /\ _)
  (Stbx.decodeToJsonString txHex)

txJsonStringToTxData :: (HexStr /\ String) -> Either ResponseError (HexStr /\ Json)
txJsonStringToTxData (txHex /\ decodedJsonString) = traverse
  (lmap (\error -> FailedTxJsonToTxData { txString : decodedJsonString, error : error }))
  (txHex /\ jsonParser decodedJsonString)

txDataToTxSum :: (HexStr /\ Json) -> Either ResponseError (HexStr /\ TxSum)
txDataToTxSum (txHex /\ txJson) = traverse
  (lmap (\error -> FailedTxDataToTxSum {txData : txJson, error : error }))
  (txHex /\ decodeTxSum txJson)
