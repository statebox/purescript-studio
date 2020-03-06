module Web.Firestore.DocumentValue where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Foreign.Object (Object)

import Web.Firestore.PrimitiveValue (PrimitiveValue)

newtype MapValue = MapValue (Object DocumentValue)

instance encodeJsonMapValue :: EncodeJson MapValue where
  encodeJson (MapValue obj) = encodeJson obj

instance decodeJsonMapValue :: DecodeJson MapValue where
  decodeJson json = MapValue <$> decodeJson json

instance showMapValue :: Show MapValue where
  show (MapValue obj) = show obj

data ArrayEntry
  = PrimitiveArrayValue PrimitiveValue
  | MapArrayValue MapValue

instance encodeJsonArrayEntry :: EncodeJson ArrayEntry where
  encodeJson = case _ of
    PrimitiveArrayValue pv -> encodeJson pv
    MapArrayValue       mv -> encodeJson mv

instance showArrayEntry :: Show ArrayEntry where
  show = case _ of
    PrimitiveArrayValue pv -> show pv
    MapArrayValue       mv -> show mv

instance decodeJsonArrayEntry :: DecodeJson ArrayEntry where
  decodeJson json
    =   (PrimitiveArrayValue <$> decodeJson json)
    <|> (MapArrayValue       <$> decodeJson json)

newtype ArrayValue = ArrayValue (Array ArrayEntry)

instance decodeJsonArrayValue :: DecodeJson ArrayValue where
  decodeJson json = ArrayValue <$> decodeJson json

data DocumentValue
  = PrimitiveDocument PrimitiveValue
  | MapDocument MapValue
  | ArrayDocument ArrayValue

instance encodeJsonDocumentValue :: EncodeJson DocumentValue where
  encodeJson = case _ of
    PrimitiveDocument primitiveValue     -> encodeJson primitiveValue
    MapDocument       (MapValue obj)     -> encodeJson obj
    ArrayDocument     (ArrayValue array) -> encodeJson array

instance decodeJsonDocumentValue :: DecodeJson DocumentValue where
  decodeJson json
    =   (PrimitiveDocument <$> decodeJson json)
    <|> (MapDocument       <$> decodeJson json)
    <|> (ArrayDocument     <$> decodeJson json)

instance showDocumentValue :: Show DocumentValue where
  show = case _ of
    PrimitiveDocument primitiveValue     -> show primitiveValue
    MapDocument       (MapValue obj)     -> show obj
    ArrayDocument     (ArrayValue array) -> show array
