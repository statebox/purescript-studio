module Web.Firestore where

import Control.Promise (Promise)
import Data.ByteString (ByteString)
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.PreciseDateTime (PreciseDateTime)
import Data.Unit (Unit)
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

newtype Lon = Lon Number

data PrimitiveValue
  = PVBoolean Boolean
  | PVBytes ByteString
  -- | Firestore is precise only up to microseconds
  | PVDateTime PreciseDateTime
  | PVFloat Number
  | PVGeographicalPoint Lat Lon
  | PVInteger Int
  | PVNull
  | PVReference String
  | PVText String

data MapValue = MapValue (Object DocumentValue)

data ArrayEntry
  = PrimitiveArrayValue PrimitiveValue
  | MapArrayValue MapValue

data ArrayValue = ArrayValue (Array ArrayEntry)

data DocumentValue
  = PrimitiveDocument PrimitiveValue
  | MapDocument MapValue
  | ArrayDocument ArrayValue

newtype DocumentData = DocumentData (Object DocumentValue)

newtype DocumentReference a = DocumentReference a

foreign import docImpl :: Fn2 Firestore String (DocumentReference DocumentData)

doc :: Firestore -> String -> DocumentReference DocumentData
doc = runFn2 docImpl

type Merge = Boolean

newtype FieldPath = FieldPath (Array String)

newtype MergeFields = MergeFields (String \/ FieldPath)

data SetOptions = SetOptions Merge MergeFields

foreign import setImpl :: forall a. Fn3 (DocumentReference a) a (Maybe SetOptions) (Promise Unit)

set :: forall a. DocumentReference a -> a -> Maybe SetOptions -> Promise Unit
set = runFn3 setImpl

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
