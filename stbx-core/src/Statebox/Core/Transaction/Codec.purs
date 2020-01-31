module Statebox.Core.Transaction.Codec where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Lens (over)
import Data.Profunctor.Choice (left)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (maybe)
import Data.NonEmpty (singleton)
import Data.Traversable (traverse)
import Foreign.Object (Object, lookup)

import Statebox.Core.Lenses (_wiring')
import Statebox.Core.Transaction (Tx, InitialTx, WiringTx, FiringTx, TxSum(..), mapTx, evalTxSum)
import Statebox.Core.Types (Net, Wiring, Firing)
import Statebox.Core.Wiring as Wiring
import Statebox.Core.Wiring (WiringRaw)

decodeTxTxSum :: Json -> String \/ Tx TxSum
decodeTxTxSum json =
  decodeFiring' json <|> decodeWiring' json <|> decodeInitial' json
  where
    decodeInitial' :: Json -> String \/ Tx TxSum
    decodeInitial' = map (mapTx InitialTxInj) <<< decodeTxInitialTx

    decodeWiring' :: Json -> String \/ Tx TxSum
    decodeWiring' = map (mapTx WiringTxInj) <<< decodeTxWiringTx

    decodeFiring' :: Json -> String \/ Tx TxSum
    decodeFiring' = map (mapTx FiringTxInj) <<< decodeTxFiringTx

decodeTxInitialTx :: Json -> String \/ Tx InitialTx
decodeTxInitialTx = decodeJson

decodeTxWiringTx :: Json -> String \/ Tx WiringTx
decodeTxWiringTx = decodeTxWith decodeWiringTx <=< decodeJson

decodeTxFiringTx :: Json -> String \/ Tx FiringTx
decodeTxFiringTx = decodeTxWith decodeFiringTx <=< decodeJson

--------------------------------------------------------------------------------

decodeInitialTx :: Json -> String \/ InitialTx
decodeInitialTx = decodeJson

decodeWiringTx :: Json -> String \/ WiringTx
decodeWiringTx = decodeJson >=> \x -> do
  wiring   <- getFieldWith decoder x "wiring"
  previous <- x .: "previous"
  pure { wiring, previous }
  where
    decoder = decodeJson >=> decodeWiring

decodeFiringTx :: Json -> String \/ FiringTx
decodeFiringTx = decodeJson >=> \x -> do
  firing   <- getFieldWith decodeFiring x "firing"
  previous <- x .: "previous"
  pure { firing, previous }

decodeTxSum :: Json -> String \/ TxSum
decodeTxSum json =
  FiringTxInj  <$> decodeFiringTx  json <|>
  WiringTxInj  <$> decodeWiringTx  json <|>
  InitialTxInj <$> decodeInitialTx json

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

decodeFiring :: Json -> String \/ Firing
decodeFiring = decodeJson >=> \x -> do
  message   <- x .:? "message"
  execution <- x .:? "execution"
  path      <- x .:  "path" >>= case _ of [y] -> Right (singleton y)
                                          _   -> Left "The 'path' field in a firing must be a singleton array."
  pure { message, execution, path }

--------------------------------------------------------------------------------

decodeWiring :: Object Json -> String \/ Wiring
decodeWiring = map Wiring.fromWiringRaw <<< decodeWiringRaw

decodeWiringRaw :: Object Json -> String \/ WiringRaw
decodeWiringRaw x = do
  nets     <- getFieldWith (netsDecoder) x "nets"
  diagrams <- x .: "diagrams"
  labels   <- x .: "labels"
  pure { nets, diagrams, labels }
  where
    netsDecoder = decodeJson >=> decodeJArray >=> traverse (decodeJson >=> decodeNet)

decodeNet :: Object Json -> String \/ Net
decodeNet x = do
  name       <- x .:  "name"
  partition  <- x .:  "partition"
  names      <- x .:  "names"
  placeNames <- x .:? "placeNames"
  pure { name, partition, names, placeNames }

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
  left msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m

--------------------------------------------------------------------------------

encodeTxSum :: TxSum -> Json
encodeTxSum = evalTxSum
  (\_ -> jsonEmptyObject) -- TODO do we want to encode/decode this?
  (\i -> encodeJson i)
  (\w -> encodeJson <<< over _wiring' Wiring.toWiringRaw $ w)
  (\f -> encodeJson f)

encodeTxWith :: forall a. (a -> Json) -> Tx a -> Json
encodeTxWith encodeBody t =
     "status"  := t.status
  ~> "hash"    := t.hash
  ~> "hex"     := t.hex
  ~> "decoded" := encodeBody t.decoded
  ~> jsonEmptyObject
