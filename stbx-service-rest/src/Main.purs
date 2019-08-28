-- - https://discourse.purescript.org/t/why-are-there-two-libraries-named-codec-argonaut-and-argonaut-codecs-what-is-the-difference-between-them/857
--
-- TODO error the out if empty transactionsDictionary in initialState?
--
-- TODO getTransactionHandler:
--      - hier moet ie idd gewrapt in een Tx met hash, status, decoded, etc velden
--        Marco had (Tx Nogwat), maar kunnen we (Tx TxSum) doen?
-- TODO van al die decodeTxUnit helpers willen we eigenlijk af
--
-- - TODO de studio geeft heel geen error als er een JSON pakketje binnenkomt met velden die nergens op slaan
--
-- - TODO module 'Codec' ertussenuit en dir omhoog en dan deze dir wegtaffen?
--        module Statebox.Core.Transaction.Codec.Encode where
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
import Statebox.Service.Model (TransactionDictionary, getTransaction, inMemoryActions)
import Statebox.Core.Transaction.Codec (encodeTxWith, encodeTxSum)

import ExampleData as Ex

-- import body parser
foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

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

-- | `getTransaction` endpoint
-- | responds to `GET /tx/<hash>`
getTransactionHandler :: AppState -> Handler
getTransactionHandler appState = do
-- TODO CORS
-- TODO CORS
-- TODO CORS
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
        Nothing /\ _ -> sendJson { status: statusToString TxNotFound
                                 , hash: hash
                                 }

-- | `postTransaction` endpoint
-- | responds to `POST /tx`
postTransactionHandler :: AppState -> Handler
postTransactionHandler appState = do
  fBody :: F (Array String) <- getBody
  case runExcept fBody of
    Left errors -> sendJson { status: statusToString NotOk
                            , errors: show errors
                            }
    Right hash  -> sendJson { status: statusToString Ok }

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
data Status = Ok | NotOk | TxNotFound

instance showStatus :: Show Status where
  show = case _ of
    Ok         -> "Ok"
    NotOk      -> "NotOk"
    TxNotFound -> "TxNotFound"

statusToString :: Status -> String
statusToString = show -- TODO this should be a JSON-compatible value; perhaps a regular JSON encoder
