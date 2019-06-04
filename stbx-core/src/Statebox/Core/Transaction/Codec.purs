module Statebox.Core.Transaction.Codec where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Control.Alt ((<|>))
import Data.Either.Nested (type (\/))

import Statebox.Core.Transaction (Tx, WiringTx, FiringTx, TxSum(..), HashStr)


decodeTxSum :: HashStr -> Json -> DecodingError \/ TxSum
decodeTxSum hash json =
  lmap DecodingError (decodeWiring json <|> decodeFiring json)
  where
    decodeWiring :: Json -> String \/ TxSum
    decodeWiring json = WiringTxInj <<< _.decoded <$> decodeJson json :: String \/ Tx WiringTx

    decodeFiring :: Json -> String \/ TxSum
    decodeFiring json = FiringTxInj <<< _.decoded <$> decodeJson json :: String \/ Tx FiringTx

newtype DecodingError = DecodingError String

instance showDecodingError :: Show DecodingError where
  show = case _ of
    DecodingError e -> "(DecodingError " <> show e <> ")"

