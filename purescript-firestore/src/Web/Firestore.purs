module Web.Firestore where

import Prelude
import Control.Promise (Promise)
import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonNull)
import Data.ByteString (ByteString)
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.PreciseDateTime (PreciseDateTime)
import Foreign.Object (Object)

type Options =
  { apiKey :: Maybe String
  , appId :: Maybe String
  , authDomain :: Maybe String
  , databaseUrl :: Maybe String
  , measurementId :: Maybe String -- TODO: should this be there or not?
  , messagingSenderId :: Maybe String
  , projectId :: String
  , storageBucket :: Maybe String
  }

data App = App
  { name :: String
  , options :: Options
  }

data Firestore = Firestore

foreign import initializeAppImpl :: Fn2 Options (Maybe String) App

initializeApp :: Options -> Maybe String -> App
initializeApp = runFn2 initializeAppImpl

foreign import firestoreImpl :: Fn1 App Firestore

firestore :: App -> Firestore
firestore = runFn1 firestoreImpl

newtype Lat = Lat Number

instance encodeJsonLat :: EncodeJson Lat where
  encodeJson (Lat n) = encodeJson n

newtype Lon = Lon Number

instance encodeJsonLon :: EncodeJson Lon where
  encodeJson (Lon n) = encodeJson n

newtype FSByteString = FSByteString ByteString

instance encodeJsonFSByteString :: EncodeJson FSByteString where
  encodeJson (FSByteString bs) = encodeJson $ show bs

newtype FSPreciseDateTime = FSPreciseDateTime PreciseDateTime

instance encodeJsonFSPreciseDateTime :: EncodeJson FSPreciseDateTime where
  encodeJson (FSPreciseDateTime dt) = encodeJson $ show dt

data PrimitiveValue
  = PVBoolean Boolean
  | PVBytes FSByteString
  -- | Firestore is precise only up to microseconds
  | PVDateTime FSPreciseDateTime
  | PVFloat Number
  | PVGeographicalPoint Lat Lon
  | PVInteger Int
  | PVNull
  | PVReference String
  | PVText String

instance encodeJsonPrimitiveValue :: EncodeJson PrimitiveValue where
  encodeJson = case _ of
    PVBoolean           b       -> encodeJson b
    PVBytes             bs      -> encodeJson bs
    PVDateTime          dt      -> encodeJson dt
    PVFloat             n       -> encodeJson n
    PVGeographicalPoint lat lon -> encodeJson { lat: lat, lon: lon }
    PVInteger           i       -> encodeJson i
    PVNull                      -> jsonNull
    PVReference         s       -> encodeJson s
    PVText              s       -> encodeJson s

newtype MapValue = MapValue (Object DocumentValue)

instance encodeJsonMapValue :: EncodeJson MapValue where
  encodeJson (MapValue obj) = encodeJson obj

data ArrayEntry
  = PrimitiveArrayValue PrimitiveValue
  | MapArrayValue MapValue

instance encodeJsonArrayEntry :: EncodeJson ArrayEntry where
  encodeJson = case _ of
    PrimitiveArrayValue pv -> encodeJson pv
    MapArrayValue       mv -> encodeJson mv

newtype ArrayValue = ArrayValue (Array ArrayEntry)

data DocumentValue
  = PrimitiveDocument PrimitiveValue
  | MapDocument MapValue
  | ArrayDocument ArrayValue

instance encodeJsonDocumentValue :: EncodeJson DocumentValue where
  encodeJson = case _ of
    PrimitiveDocument primitiveValue     -> encodeJson primitiveValue
    MapDocument       (MapValue obj)     -> encodeJson obj
    ArrayDocument     (ArrayValue array) -> encodeJson array

newtype DocumentData = DocumentData (Object DocumentValue)

instance encodeJsonDocumentData :: EncodeJson DocumentData where
  encodeJson (DocumentData obj) = encodeJson obj

newtype DocumentReference a = DocumentReference a

foreign import docImpl :: Fn2 Firestore String (DocumentReference DocumentData)

doc :: Firestore -> String -> DocumentReference DocumentData
doc = runFn2 docImpl

type Merge = Boolean

newtype FieldPath = FieldPath (Array String)

newtype MergeFields = MergeFields (String \/ FieldPath)

data SetOptions = SetOptions Merge MergeFields

foreign import setImpl :: forall a. Fn3 (DocumentReference a) Json (Maybe SetOptions) (Promise Unit)

set :: forall a. EncodeJson a => DocumentReference a -> a -> Maybe SetOptions -> Promise Unit
set docRef a options = runFn3 setImpl docRef (encodeJson a) options

data SourceOption
  = Default
  | Server
  | Cache

newtype GetOptions = GetOptions SourceOption

newtype DocumentSnapshot a = DocumentSnapshot a

foreign import getImpl :: forall a. Fn2 (DocumentReference a) (Maybe GetOptions) (Promise (DocumentSnapshot a))

get :: forall a. DocumentReference a -> Maybe GetOptions -> Promise (DocumentSnapshot a)
get = runFn2 getImpl

data ServerTimestamps
  = Estimate
  | Previous
  | None

data SnapshotOptions = SnapshotOptions ServerTimestamps

foreign import dataImpl :: forall a. Fn2 (DocumentSnapshot a) (Maybe SnapshotOptions) (Maybe a)

snapshotData :: forall a. DocumentSnapshot a -> Maybe SnapshotOptions -> Maybe a
snapshotData = runFn2 dataImpl
