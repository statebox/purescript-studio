module Statebox.Core.Transaction.Codec where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object(..), lookup)

import Statebox.Core.Transaction (Tx, InitialTx, WiringTx, FiringTx, TxSum(..), HashStr, mapTx)
import Statebox.Core.Types (Firing)

-- | NOTA BENE: This produces a `TxSum`, but it expects a JSON structure corresponding to `Tx TxSum`, and then takes
-- | the `Tx`'s `decoded` field and wraps that as the returned `TxSum`.
decodeTxSum :: Json -> DecodingError \/ TxSum
decodeTxSum = bimap DecodingError (_.decoded) <<< decodeTxTxSum

decodeTxTxSum :: Json -> String \/ Tx TxSum
decodeTxTxSum json =
  decodeFiring json <|> decodeWiring json <|> decodeInitial json
  where
    decodeInitial :: Json -> String \/ Tx TxSum
    decodeInitial = map (mapTx InitialTxInj) <<< decodeTxInitialTx

    decodeWiring :: Json -> String \/ Tx TxSum
    decodeWiring = map (mapTx WiringTxInj) <<< decodeTxWiringTx

    decodeFiring :: Json -> String \/ Tx TxSum
    decodeFiring = map (mapTx FiringTxInj) <<< decodeTxFiringTx

decodeTxInitialTx :: Json -> String \/ Tx InitialTx
decodeTxInitialTx = decodeJson

decodeTxWiringTx :: Json -> String \/ Tx WiringTx
decodeTxWiringTx = decodeJson

decodeTxFiringTx :: Json -> String \/ Tx FiringTx
decodeTxFiringTx = decodeTxWith decodeFiringTx' <=< decodeJson
  where
    decodeFiringTx' :: Json -> String \/ FiringTx
    decodeFiringTx' = decodeFiringTx <=< decodeJson

--------------------------------------------------------------------------------

newtype DecodingError = DecodingError String

instance eqDecodingError :: Eq DecodingError where
  eq (DecodingError x) (DecodingError y) = x == y

instance showDecodingError :: Show DecodingError where
  show = case _ of
    DecodingError e -> "(DecodingError " <> show e <> ")"

--------------------------------------------------------------------------------

-- | The 'body' of a `Tx` envelope is in the `decoded` field. This field (of type `a`) is
-- | polymorphic, and you can specify a decoder for it.
decodeTxWith :: ∀ a. (Json -> String \/ a) -> Object Json -> String \/ Tx a
decodeTxWith aDecoder x = do
  status  <- x .: "status"
  hash    <- x .: "hash"
  hex     <- x .: "hex"
  decoded <- getFieldWith aDecoder x "decoded"
  pure { status, hash, hex, decoded }

--------------------------------------------------------------------------------

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
getFieldWith :: ∀ a. (Json -> String \/ a) -> Object Json -> String -> String \/ a
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
