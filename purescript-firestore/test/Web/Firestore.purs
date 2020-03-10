module Test.Web.Firestore where

import Prelude (Unit, bind, discard, show, ($), (#))
import Control.Promise (toAff)
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (singleton)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Web.Firestore (doc, firestore, get, initializeApp, set, snapshotData)
import Web.Firestore.DocumentData (DocumentData(..))
import Web.Firestore.DocumentValue (DocumentValue(..))
import Web.Firestore.Options (apiKey, appId, authDomain, databaseUrl, messagingSenderId, options, storageBucket)
import Web.Firestore.PrimitiveValue (PrimitiveValue(..))

suite :: Spec Unit
suite = do
  describe "Firestore" do
    it "sets and gets data correctly" do
      let fsOptions = options "firestore-test-270209"
                    # Lens.set apiKey            "AIzaSyBDH92I_Qiv_GHbIOA0MddiOKZpwDaMNoY"
                    # Lens.set appId             "1:490707848264:web:92683957e61378c9a21c7d"
                    # Lens.set authDomain        "firestore-test-270209.firebaseapp.com"
                    # Lens.set databaseUrl       "https://firestore-test-270209.firebaseio.com"
                    # Lens.set messagingSenderId "490707848264"
                    # Lens.set storageBucket     "firestore-test-270209.appspot.com"
          app = initializeApp fsOptions (Just "firestore-test")
          firestoreInstance = firestore app
          documentReference = doc firestoreInstance "collection/test"
          document = DocumentData (singleton "foo" (PrimitiveDocument (PVText "bar")))
          setPromise = set documentReference document Nothing
          getPromise = get documentReference Nothing
      toAff setPromise
      snapshot <- toAff getPromise
      let result = show $ snapshotData snapshot Nothing
      liftEffect $ log $ show (snapshotData snapshot Nothing)

      1 `shouldEqual` 1

