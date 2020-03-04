module Web.Firestore where

import Prelude
import Control.Alt ((<|>))
import Control.Promise (Promise)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, caseJsonNull, decodeJson, encodeJson, fromString, jsonNull)
import Data.ByteString (ByteString)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..))
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

newtype FSByteString = FSByteString ByteString

instance encodeJsonFSByteString :: EncodeJson FSByteString where
  encodeJson (FSByteString bs) = encodeJson $ show bs

instance showFSByteString :: Show FSByteString where
  show (FSByteString bs) = show bs

newtype FSPreciseDateTime = FSPreciseDateTime PreciseDateTime

instance encodeJsonFSPreciseDateTime :: EncodeJson FSPreciseDateTime where
  encodeJson (FSPreciseDateTime dt) = encodeJson $ show dt

instance showFSPreciseDateTime :: Show FSPreciseDateTime where
  show (FSPreciseDateTime dt) = show dt

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

instance decodeJsonPrimitiveValue :: DecodeJson PrimitiveValue where
  decodeJson json
    =   (PVBoolean           <$> decodeJson json)
    -- <|> (PVBytes             <$> decodeJson json) -- TODO: how do I decode bytes?
    -- <|> (PVDateTime          <$> decodeJson json) -- TODO: how doe we parse a DateTime? how is it represented in Firestore?
    <|> (PVFloat             <$> decodeJson json)
    <|> ((\({lat: lat, lon: lon} :: {lat:: Lat, lon:: Lon}) -> PVGeographicalPoint lat lon) <$> decodeJson json)
    <|> (PVInteger           <$> decodeJson json)
    <|> (caseJsonNull (Left "not null") (const $ Right PVNull) json)
    <|> (PVReference         <$> decodeJson json) -- TODO: parse only strings with the correct format
    <|> (PVText              <$> decodeJson json)

instance showPrimitiveValue :: Show PrimitiveValue where
  show = case _ of
    PVBoolean           b       -> show b
    PVBytes             bs      -> show bs
    PVDateTime          dt      -> show dt
    PVFloat             n       -> show n
    PVGeographicalPoint lat lon -> show { lat: lat, lon: lon }
    PVInteger           i       -> show i
    PVNull                      -> "null"
    PVReference         s       -> show s
    PVText              s       -> show s

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

newtype DocumentData = DocumentData (Object DocumentValue)

instance encodeJsonDocumentData :: EncodeJson DocumentData where
  encodeJson (DocumentData obj) = encodeJson obj

instance decodeJsonDocumentData :: DecodeJson DocumentData where
  decodeJson = (DocumentData <$> _) <<< decodeJson

instance showDocumentData :: Show DocumentData where
  show (DocumentData obj) = show obj

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

instance encodeJsonSourceOption :: EncodeJson SourceOption where
  encodeJson = case _ of
    Default -> fromString "default"
    Server  -> fromString "Server"
    Cache   -> fromString "Cache"

newtype GetOptions = GetOptions SourceOption

instance encodeJsonGetOptions :: EncodeJson GetOptions where
  encodeJson (GetOptions so) = encodeJson so

newtype DocumentSnapshot a = DocumentSnapshot a

foreign import getImpl :: forall a. Fn2 (DocumentReference a) Json (Promise (DocumentSnapshot a))

get :: forall a. DocumentReference a -> Maybe GetOptions -> Promise (DocumentSnapshot a)
get docRef options = (runFn2 getImpl) docRef (encodeJson options)

data ServerTimestamps
  = Estimate
  | Previous
  | None

data SnapshotOptions = SnapshotOptions ServerTimestamps

foreign import dataImpl :: forall a. Fn2 (DocumentSnapshot a) (Maybe SnapshotOptions) Json

snapshotData :: forall a. DecodeJson a => DocumentSnapshot a -> Maybe SnapshotOptions -> Maybe a
snapshotData docSnapshot options = either (const Nothing) identity (decodeJson $ runFn2 dataImpl docSnapshot options)
