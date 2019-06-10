module Statebox.Client where

import Prelude
import Affjax as Affjax
import Affjax (URL, Response)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.RequestHeader as RequestHeader
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.HTTP.Method (Method(GET))
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashStr, TxSum(..), isUberRootHash)
import Statebox.Core.Transaction.Codec (decodeTxSum, DecodingError)

requestTransaction :: URL -> HashStr -> Aff (ResponseFormatError \/ (DecodingError \/ TxSum))
requestTransaction apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right <<< Right $ UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    pure $ decodeTxSum hash <$> res.body

requestTransactionJson :: URL -> HashStr -> Aff (Response (ResponseFormatError \/ Json))
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }
