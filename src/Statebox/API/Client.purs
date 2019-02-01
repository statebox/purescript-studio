module Statebox.API.Client where

import Prelude
import Affjax as Affjax
import Affjax (Response, URL)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.RequestHeader as RequestHeader
import Affjax.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Effect.Aff (Aff)

import Statebox.API.Types

requestTransaction :: URL -> HashStr -> Aff (Response (Either ResponseFormatError Json))
requestTransaction apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/get/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }
