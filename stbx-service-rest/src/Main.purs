module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.State.Trans (runStateT)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read)
import Effect.Exception (Error, error, message)
import Foreign (F)
import Node.Express.App (App, listenHttp, get, post, use, useExternal, useOnError)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getBody, getRouteParam, getOriginalUrl)
import Node.Express.Response (sendJson, setStatus, setResponseHeader)
import Node.Express.Types (Request, Response)
import Node.HTTP (Server)

import Statebox.Core.Transaction (TxId, TxSum)
import Statebox.Service.Model (LeTx, TransactionDictionary, getTransaction, inMemoryActions)
import Statebox.Core.Transaction.Codec.Encode (encodeTxWith, encodeTxSum)

import ExampleData as Ex

-- import body parser
foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

-- TODO: move this elsewhere
endpointUrl :: String
endpointUrl = "https://testapi.statebox.io"

stbxPort :: Int
stbxPort = 8080

-- application state

type AppState = Ref TransactionDictionary

initialState :: Effect AppState
initialState = new $ either mempty identity Ex.txIdsDictE

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

-- | `getTransaction` endpoint
-- | responds to `GET /tx/<hash>`
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
          { status: "ok"
          , hash: hash
          , hex: "TODO"
          , decoded: transaction
          }
        Nothing          /\ _ -> sendJson { hash: hash
                                          , error: show TxNotFound
                                          }

-- | `postTransaction` endpoint
-- | responds to `POST /tx`
postTransactionHandler :: AppState -> Handler
postTransactionHandler appState = do
  fBody :: F (Array String) <- getBody
  case runExcept fBody of
    Left errors -> sendJson { status: "ko"
                            , errors : show errors
                            }
    Right hash  -> sendJson { status: "ok" }

-- application definition with routing

app :: AppState -> App
app state = do
  useExternal         jsonBodyParser
  use                 logger
  get  "/"            index
  get  "/healthcheck" healthcheck
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
data Err = TxNotFound

instance showErr :: Show Err where
  show TxNotFound = "TxNotFound"
