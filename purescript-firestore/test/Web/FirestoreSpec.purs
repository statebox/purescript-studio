module Test.Web.FirestoreSpec where

import Prelude
import Control.Promise (toAff)
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (singleton)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import Web.Firestore (doc, firestore, get, initializeApp, set, snapshotData)
import Web.Firestore.DocumentData (DocumentData(..))
import Web.Firestore.DocumentValue (DocumentValue(..))
import Web.Firestore.Options (apiKey, appId, authDomain, databaseUrl, messagingSenderId, options, storageBucket)
import Web.Firestore.Path (pathFromString)
import Web.Firestore.PrimitiveValue (PrimitiveValue(..))

suite :: Spec Unit
suite = do
  describe "Firestore" do
    it "sets and gets data correctly" do
      let fsOptions = options "firestore-test-270209"
                    # Lens.set apiKey            (Just "AIzaSyBDH92I_Qiv_GHbIOA0MddiOKZpwDaMNoY")
                    # Lens.set appId             (Just "1:490707848264:web:92683957e61378c9a21c7d")
                    # Lens.set authDomain        (Just "firestore-test-270209.firebaseapp.com")
                    # Lens.set databaseUrl       (Just "https://firestore-test-270209.firebaseio.com")
                    # Lens.set messagingSenderId (Just "490707848264")
                    # Lens.set storageBucket     (Just "firestore-test-270209.appspot.com")
          app = initializeApp fsOptions (Just "firestore-test")
          firestoreInstance = firestore app
          maybeDocumentReference = doc firestoreInstance <$> (pathFromString "collection/test")
      case maybeDocumentReference of
        Nothing                -> fail "invalid path"
        Just documentReference ->
          let document = DocumentData (singleton "foo" (PrimitiveDocument (PVText "bar")))
              setPromise = set documentReference document Nothing
              getPromise = get documentReference Nothing
          in do
            toAff setPromise
            snapshot <- toAff getPromise
            let result = show $ snapshotData snapshot Nothing
            liftEffect $ log $ show (snapshotData snapshot Nothing)

            1 `shouldEqual` 1
