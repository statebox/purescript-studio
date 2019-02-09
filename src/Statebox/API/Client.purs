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

import Statebox.API.Types
import Statebox.API.Types (FiringOrWiring)

requestTransaction :: URL -> HashStr -> Aff (ResponseFormatError \/ (DecodingError \/ FiringOrWiring))
requestTransaction apiBaseUrl hash = do
  res <- requestTransactionJson apiBaseUrl hash
  pure $ decodeFiringOrWiring <$> res.body

requestTransactionJson :: URL -> HashStr -> Aff (Response (ResponseFormatError \/ Json))
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/get/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

--------------------------------------------------------------------------------

newtype DecodingError = DecodingError String

decodeFiringOrWiring :: Json -> DecodingError \/ FiringOrWiring
decodeFiringOrWiring json =
  lmap DecodingError (decodeWiring json <|> decodeFiring json)
  where
    decodeWiring :: Json -> String \/ FiringOrWiring
    decodeWiring json = LeWiring <<< _.decoded <$> decodeJson json :: String \/ Tx WiringTx

    decodeFiring :: Json -> String \/ FiringOrWiring
    decodeFiring json = LeFiring <<< _.decoded <$> decodeJson json :: String \/ Tx FiringTx
