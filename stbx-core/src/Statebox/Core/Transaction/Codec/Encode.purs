module Statebox.Core.Transaction.Codec.Encode where

import Prelude
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((:=), (:=?), (~>), (~>?))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Statebox.Core.Transaction (Tx, TxSum(..), evalTxSum)

encodeTxSum :: TxSum -> Json
encodeTxSum = evalTxSum
  (\_ -> "TODO_uber" := 42 ~> jsonEmptyObject)
  (\i -> encodeJson i)
  (\w -> encodeJson w)
  (\f -> encodeJson f)

encodeTxWith :: forall a. (a -> Json) -> Tx a -> Json
encodeTxWith encodeBody t =
     "status"  := t.status
  ~> "hash"    := t.hash
  ~> "hex"     := t.hex
  ~> "decoded" := encodeBody t.decoded
  ~> jsonEmptyObject
