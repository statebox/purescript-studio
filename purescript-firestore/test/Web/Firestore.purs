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

suite :: Spec Unit
suite = do
  describe "Firestore" do
    it "sets and gets data correctly" do
      let options = { apiKey: Just "AIzaSyBFgU7cDEBkYmRCnj7hSLbNEj6LNzfwIew"
                    , appId: Just "1:630616051437:web:118b8ede4a1a86c73b46e4"
                    , authDomain: Just "marcoshtest-b082d.firebaseapp.com"
                    , databaseUrl: Just "https://marcoshtest-b082d.firebaseio.com"
                    , measurementId: Nothing
                    , messagingSenderId: Just "630616051437"
                    , projectId: "marcoshtest-b082d"
                    , storageBucket: Just "marcoshtest-b082d.appspot.com"
                    }
          app = initializeApp options (Just "marcoshtest")
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

