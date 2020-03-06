module Web.Firestore.LatLon where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)

newtype Lat = Lat Number

instance encodeJsonLat :: EncodeJson Lat where
  encodeJson (Lat n) = encodeJson n

instance decodeJsonLat :: DecodeJson Lat where
  decodeJson json = Lat <$> decodeJson json

instance showLat :: Show Lat where
  show (Lat n) = show n

newtype Lon = Lon Number

instance encodeJsonLon :: EncodeJson Lon where
  encodeJson (Lon n) = encodeJson n

instance decodeJsonLon :: DecodeJson Lon where
  decodeJson json = Lon <$> decodeJson json

instance showLon :: Show Lon where
  show (Lon n) = show n
