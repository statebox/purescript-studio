module Web.Firestore.SetOptions where

import Prelude
import Data.Argonaut (class EncodeJson, encodeJson, fromArray, jsonEmptyObject, (:=), (~>))
import Data.Either (either)
import Data.Either.Nested (type (\/))

type Merge = Boolean

foreign import data FieldPath :: Type

foreign import buildFieldPath :: Array String -> FieldPath

foreign import fieldNames :: FieldPath -> Array String

instance encodeJsonFieldPath :: EncodeJson FieldPath where
  encodeJson fieldPath = encodeJson $ fieldNames fieldPath

newtype MergeFields = MergeFields (Array (String \/ FieldPath))

instance encodeJsonMergeFields :: EncodeJson MergeFields where
  encodeJson (MergeFields fields) = fromArray $ either encodeJson encodeJson <$> fields

data SetOptions
  = MergeOption Merge
  | MergeFieldsOption MergeFields

instance encodeJsonSetOptions :: EncodeJson SetOptions where
  encodeJson = case _ of
    MergeOption       merge       -> "merge"       := encodeJson merge       ~> jsonEmptyObject
    MergeFieldsOption mergeFields -> "mergeFields" := encodeJson mergeFields ~> jsonEmptyObject
