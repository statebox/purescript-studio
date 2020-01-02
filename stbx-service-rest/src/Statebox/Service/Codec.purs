module Statebox.Service.Codec where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Either.Nested (type (\/))

import Statebox.Service (TxErrorResponseBody)

decodeTxErrorResponseBody :: Json -> String \/ TxErrorResponseBody
decodeTxErrorResponseBody = decodeJson >=> \x -> do
  status  <- x .: "status"
  code    <- x .: "code"
  message <- x .: "message"
  pure { status, code, message }