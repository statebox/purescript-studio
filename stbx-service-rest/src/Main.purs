module Main where

import Prelude

import Control.Monad.State.Trans (runStateT)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import Effect.Exception (Error, error, message)
import Node.Express.App (App, listenHttp, get, post, use, useExternal, useOnError)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getBody', getRouteParam, getOriginalUrl)
import Node.Express.Response (sendJson, setStatus, setResponseHeader)
import Node.Express.Types (Request, Response)
import Node.HTTP (Server)
import Unsafe.Coerce (unsafeCoerce)

import Statebox.Core.Transaction.Codec (decodeTxSum, encodeTxWith, encodeTxSum)
import Statebox.TransactionStore.Types (getTransaction, putTransaction)
import Statebox.TransactionStore.Memory (TransactionDictionary, inMemoryActions, encodeTransactionDictionary)

import ExampleData as Ex

foreign import stringBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

stbxPort :: Int
stbxPort = 8080

-- application state

type AppState = Ref TransactionDictionary

initialState :: Effect AppState
initialState = new $ either mempty identity Ex.transactionsDictionary

-- middleware

logger :: Handler
logger = do
  url <- getOriginalUrl
  liftEffect $ log (">>> " <> url)
  next

errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson { error: message err }

-- handlers

index :: Handler
index = sendJson
  { name        : "Statebox REST API"
  , description : "collection of endpoints to interact with the Statebox protocol"
  , endpoints   : { healthcheck    : "/healthcheck"
                  , getTransaction : "/tx/:hash"
                  }
  }

healthcheck :: Handler
healthcheck = sendJson { health: "I'm fine" }

getTransactionsHandler :: AppState -> Handler
getTransactionsHandler appState = do
  setResponseHeader "Access-Control-Allow-Origin" "*"
  transactionDictionary <- liftEffect $ read appState
  sendJson { status: "ok"
           , transactions: encodeTransactionDictionary transactionDictionary
           }

-- | Endpoint for the `getTransaction` action on the transaction storage.
-- | Responds to `GET /tx/<hash>`.
getTransactionHandler :: AppState -> Handler
getTransactionHandler appState = do
  setResponseHeader "Access-Control-Allow-Origin" "*"
  maybeHash <- getRouteParam "hash"
  case maybeHash of
    Nothing   -> nextThrow $ error "Hash is required"
    Just hash -> do
      transactionDictionary <- liftEffect $ read appState
      maybeTransaction <- liftAff $ runStateT (inMemoryActions $ getTransaction hash) transactionDictionary
      case maybeTransaction of
        Just transaction /\ _ -> sendJson $ encodeTxWith encodeTxSum
          { status: statusToString Ok
          , hash: hash
          , hex: "TODO" -- TODO #237
          , decoded: transaction
          }
        Nothing /\ _ -> sendJson
          { status: statusToString TxNotFound
          , hash: hash
          }

-- | Endpoint for the `postTransaction` action on the transaction storage.
-- | Responds to `POST /tx`.
postTransactionHandler :: AppState -> Handler
postTransactionHandler appState = do
  -- TODO: find a proper way to manage body decoding
  body :: String <- unsafeCoerce <$> getBody'
  case jsonParser body of
    Left error -> sendJson
      { status : statusToString NotOk
      , error  : error
      }
    Right json -> do
      case decodeTxSum json of
        Left error -> sendJson
          { status : statusToString NotOk
          , error  : error
          }
        Right txSum -> do
          transactionDictionary <- liftEffect $ read appState
          updatedTransactionDictionary <- liftAff $ runStateT (inMemoryActions $ putTransaction "new-hash" txSum) transactionDictionary
          liftEffect $ write (snd updatedTransactionDictionary) appState
          sendJson { status: stringify json }

-- application definition with routing

app :: AppState -> App
app state = do
  useExternal         stringBodyParser
  use                 logger
  get  "/"            index
  get  "/healthcheck" healthcheck
  get  "/tx"          (getTransactionsHandler state)
  get  "/tx/:hash"    (getTransactionHandler state)
  post "/tx"          (postTransactionHandler state)
  useOnError          errorHandler

-- run application

main :: Effect Server
main = do
  state <- initialState
  listenHttp (app state) stbxPort \_ ->
    log $ "Listening on " <> show stbxPort

--------------------------------------------------------------------------------

-- | TODO this is now used ad hoc in JSON responses; these should be made to conform to the Statebox protocol spec.
data Status = Ok | NotOk | TxNotFound

instance showStatus :: Show Status where
  show = case _ of
    Ok         -> "Ok"
    NotOk      -> "NotOk"
    TxNotFound -> "TxNotFound"

statusToString :: Status -> String
statusToString = show -- TODO this should be a JSON-compatible value; perhaps a regular JSON encoder
