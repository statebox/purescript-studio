module Web.Firestore.PreciseDateTime where

import Prelude
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.PreciseDateTime (PreciseDateTime)

newtype FSPreciseDateTime = FSPreciseDateTime PreciseDateTime

instance encodeJsonFSPreciseDateTime :: EncodeJson FSPreciseDateTime where
  encodeJson (FSPreciseDateTime dt) = encodeJson $ show dt

instance showFSPreciseDateTime :: Show FSPreciseDateTime where
  show (FSPreciseDateTime dt) = show dt
