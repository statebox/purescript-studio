
module Statebox.Core.Transaction.Codec where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object(..), lookup)

import Statebox.Core.Transaction (Tx, InitialTx, WiringTx, FiringTx, TxSum(..), HashStr)
import Statebox.Core.Types (Firing)


decodeTxSum :: HashStr -> Json -> DecodingError \/ TxSum
decodeTxSum hash json =
  lmap DecodingError (decodeFiring json <|> decodeWiring json <|> decodeInitial json)
  where
    decodeInitial :: Json -> String \/ TxSum
    decodeInitial json = InitialTxInj <<< _.decoded <$> decodeJson json :: String \/ Tx InitialTx

    decodeWiring :: Json -> String \/ TxSum
    decodeWiring json = WiringTxInj <<< _.decoded <$> decodeJson json :: String \/ Tx WiringTx

    decodeFiring :: Json -> String \/ TxSum
    decodeFiring json = FiringTxInj <<< _.decoded <$> (decodeTxFiringTx =<< decodeJson json)

newtype DecodingError = DecodingError String

instance showDecodingError :: Show DecodingError where
  show = case _ of
    DecodingError e -> "(DecodingError " <> show e <> ")"

--------------------------------------------------------------------------------

decodeTxFiringTx :: Object Json -> String \/ Tx FiringTx
decodeTxFiringTx x = do
  status  <- x .:  "status"
  hex     <- x .:  "hex"
  decoded <- getFieldWith decoder x "decoded"
  pure { status, hex, decoded }
  where
    decoder = decodeJson >=> decodeFiringTx

decodeFiringTx :: Object Json -> String \/ FiringTx
decodeFiringTx x = do
  firing   <- getFieldWith decoder x "firing"
  previous <- x .:  "previous"
  pure { firing, previous }
  where
    decoder = decodeJson >=> decodeFiring

decodeFiring :: Object Json -> String \/ Firing
decodeFiring x = do
  message <- x .:? "message"
  path    <- x .:  "path"
  pure { message, path }

--------------------------------------------------------------------------------
-- Helpers taken from [Data.Argonaut.Decode.Combinators](https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/src/Data/Argonaut/Decode/Combinators.purs).
--------------------------------------------------------------------------------

-- Adapted `getField` that allows you to override the decoder, See [Argonaut.Decode.Combinators](from https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/9a1c0e09ca523ba7a290461e5346b818059f3d2a/src/Data/Argonaut/Decode/Combinators.purs#L58).
getFieldWith :: forall a. DecodeJson a => (Json -> String \/ a) -> Object Json -> String -> String \/ a
getFieldWith decoder o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decoder) -- TODO decoder was decodeJson
    (lookup s o)

-- Duplicate of https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/9a1c0e09ca523ba7a290461e5346b818059f3d2a/src/Data/Argonaut/Decode/Combinators.purs#L132.
elaborateFailure :: ∀ a. String -> String \/ a -> String \/ a
elaborateFailure s e =
  lmap msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m
