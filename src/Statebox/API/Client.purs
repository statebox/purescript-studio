module Statebox.API.Client where

import Prelude
import Affjax as Affjax
import Affjax (Response)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.RequestHeader as RequestHeader
import Affjax.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Affjax.StatusCode (StatusCode(..))
import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.HTTP.Method (Method(GET))
import Effect.Aff (Aff)

import Statebox.API.Transaction (HashStr, Tx, TxSum(..), WiringTx, FiringTx, namespaceRootHash_HACK)

requestTransaction :: URL -> HashStr -> Aff (ResponseFormatError \/ (DecodingError \/ TxSum))
requestTransaction apiBaseUrl hash =
  if hash == namespaceRootHash_HACK then
    pure $ Right <<< Right $ LeInitial hash
  else do
    res <- requestTransactionJson apiBaseUrl hash
    pure $ decodeTxSum hash <$> res.body

requestTransactionJson :: URL -> HashStr -> Aff (Response (ResponseFormatError \/ Json))
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

--------------------------------------------------------------------------------

newtype DecodingError = DecodingError String

instance showDecodingError :: Show DecodingError where
  show = case _ of
    DecodingError e -> "(DecodingError " <> show e <> ")"

decodeTxSum :: HashStr -> Json -> DecodingError \/ TxSum
decodeTxSum hash json =
  lmap DecodingError (decodeWiring json <|> decodeFiring json)
  where
    decodeWiring :: Json -> String \/ TxSum
    decodeWiring json = LeWiring <<< _.decoded <$> decodeJson json :: String \/ Tx WiringTx

    decodeFiring :: Json -> String \/ TxSum
    decodeFiring json = LeFiring <<< _.decoded <$> decodeJson json :: String \/ Tx FiringTx
