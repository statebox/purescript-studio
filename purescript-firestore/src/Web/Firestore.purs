module Web.Firestore where

import Prelude
import Control.Promise (Promise)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromString)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..))

import Web.Firestore.DocumentData (DocumentData)
import Web.Firestore.Options (Options)

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
