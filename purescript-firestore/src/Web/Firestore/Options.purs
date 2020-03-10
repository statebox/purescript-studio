module Web.Firestore.Options where

import Prelude ((>>>))
import Data.Lens (Iso', Lens', Traversal', _Just, iso, lens)
import Data.Maybe (Maybe(..))

newtype Options = Options
  { apiKey :: Maybe String
  , appId :: Maybe String
  , authDomain :: Maybe String
  , databaseUrl :: Maybe String
  , measurementId :: Maybe String -- TODO: should this be there or not?
  , messagingSenderId :: Maybe String
  , projectId :: String
  , storageBucket :: Maybe String
  }

options :: String -> Options
options pId = Options
  { apiKey : Nothing
  , appId : Nothing
  , authDomain : Nothing
  , databaseUrl : Nothing
  , measurementId : Nothing
  , messagingSenderId : Nothing
  , projectId : pId
  , storageBucket : Nothing
  }

optionsIso :: Iso' Options { apiKey :: Maybe String
                           , appId :: Maybe String
                           , authDomain :: Maybe String
                           , databaseUrl :: Maybe String
                           , measurementId :: Maybe String
                           , messagingSenderId :: Maybe String
                           , projectId :: String
                           , storageBucket :: Maybe String
                           }
optionsIso = iso (\(Options r) -> r) Options

apiKey :: Traversal' Options String
apiKey = _Just >>> lens (_.apiKey) (_ {apiKey = _}) >>> optionsIso

appId :: Traversal' Options String
appId = _Just >>> lens (_.appId) (_ {appId = _}) >>> optionsIso

authDomain :: Traversal' Options String
authDomain = _Just >>> lens (_.authDomain) (_ {authDomain = _}) >>> optionsIso

databaseUrl :: Traversal' Options String
databaseUrl = _Just >>> lens (_.databaseUrl) (_ {databaseUrl = _}) >>> optionsIso

measurementId :: Traversal' Options String
measurementId = _Just >>> lens (_.measurementId) (_ {measurementId = _}) >>> optionsIso

messagingSenderId :: Traversal' Options String
messagingSenderId = _Just >>> lens (_.messagingSenderId) (_ {messagingSenderId = _}) >>> optionsIso

projectId :: Lens' Options String
projectId = lens (_.projectId) (_ {projectId = _}) >>> optionsIso

storageBucket :: Traversal' Options String
storageBucket = _Just >>> lens (_.storageBucket) (_ {storageBucket = _}) >>> optionsIso
