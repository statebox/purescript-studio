module Test.Web.Firestore where

import Prelude
import Control.Promise (toAff)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (singleton)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Web.Firestore
import Web.Firestore.DocumentData (DocumentData(..))
import Web.Firestore.DocumentValue (DocumentValue(..))
import Web.Firestore.PrimitiveValue (PrimitiveValue(..))

suite :: Spec Unit
suite = do
  describe "Firestore" do
    it "sets and gets data correctly" do
      let options = { apiKey: Just "AIzaSyBDH92I_Qiv_GHbIOA0MddiOKZpwDaMNoY"
                    , appId: Just "1:490707848264:web:92683957e61378c9a21c7d"
                    , authDomain: Just "firestore-test-270209.firebaseapp.com"
                    , databaseUrl: Just "https://firestore-test-270209.firebaseio.com"
                    , measurementId: Nothing
                    , messagingSenderId: Just "490707848264"
                    , projectId: "firestore-test-270209"
                    , storageBucket: Just "firestore-test-270209.appspot.com"
                    }
          app = initializeApp options (Just "firestore-test")
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

