module Main where

import Prelude

import Control.Monad.State.Trans (runStateT)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (new, read, write) as Ref
import Effect.Ref (Ref)
import Effect.Exception (Error, error, message)
import Node.Express.App (App, listenHttp, get, post, use, useExternal, useOnError)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getBody', getRouteParam, getOriginalUrl)
import Node.Express.Response (sendJson, setStatus, setResponseHeader)
import Node.Express.Types (Request, Response)
import Node.HTTP (Server)
import Unsafe.Coerce (unsafeCoerce)

import Statebox.Core (decodeToJsonString, hash) as Stbx
import Statebox.Core.Transaction (Tx, TxSum(..))
import Statebox.Core.Transaction.Codec (decodeTxSum, encodeTxWith, encodeTxSum)
import Statebox.Core.Types (HexStr)
import Statebox.Service (TxError(..), txErrorCode, txErrorMessage)
import Statebox.TransactionStore (get, put) as TransactionStore
import Statebox.TransactionStore.Memory (eval) as TransactionStore.Memory
import Statebox.TransactionStore.Memory (TransactionDictionary, encodeTransactionDictionary)

foreign import stringBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

stbxPort :: Int
stbxPort = 8080

-- application state

type AppState =
  { transactionDictionaryRef :: Ref TransactionDictionary }

initialState :: Effect AppState
initialState = map { transactionDictionaryRef: _ } <$> Ref.new $ initialTransactionDictionary
  where
    initialTransactionDictionary = mempty

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
getTransactionsHandler state = do
  setResponseHeader "Access-Control-Allow-Origin" "*"
  transactionDictionary <- liftEffect $ Ref.read state.transactionDictionaryRef
  sendJson { status: statusToString Ok
           , transactions: encodeTransactionDictionary transactionDictionary
           }

-- | Endpoint for getting transactions from the transaction store.
-- | Responds to `GET /tx/<hash>`.
getTransactionHandler :: AppState -> Handler
getTransactionHandler state = do
  setResponseHeader "Access-Control-Allow-Origin" "*"
  maybeHash <- getRouteParam "hash"
  case maybeHash of
    Nothing   -> nextThrow $ error "Hash is required"
    Just hash -> do
      transactionDictionary <- liftEffect $ Ref.read state.transactionDictionaryRef
      maybeTransaction <- liftAff $ runStateT (TransactionStore.Memory.eval $ TransactionStore.get hash) transactionDictionary
      case maybeTransaction of
        Just transaction /\ _ -> sendTxTxSum
          { status: statusToString Ok
          , hash: hash
          , hex: "TODO" -- TODO #237 get from transaction store instead of computing, if possible
          , decoded: transaction
          }
        Nothing /\ _ -> sendTxError (TxNotFound { hash })

-- | Endpoint for saving transactions to the transaction store.
-- | Responds to `POST /tx`.
postTransactionHandler :: AppState -> Handler
postTransactionHandler state = do
  -- TODO: find a proper way to manage body decoding
  bodyStr :: String <- unsafeCoerce <$> getBody'
  case jsonParser bodyStr of
    Left error -> sendJson
      { status : statusToString Failed
      , error  : error
      }
    Right json -> case decodeJson json :: String \/ TxPostRequestBody of
      Left error -> sendTxError TxNoTxField
      Right (body :: TxPostRequestBody) -> do
        let txHex = body.tx
        decodedJsonString <- liftEffect $ Stbx.decodeToJsonString txHex
        case jsonParser decodedJsonString of
          Left error -> sendJson
            { status : statusToString Failed
            , error  : error
            }
          Right txJson -> case decodeTxSum txJson of
            Left error -> sendJson
              { status : statusToString Failed
              , error  : error
              }
            Right txSum -> do
              let hash = Stbx.hash txHex
              transactionDictionary <- liftEffect $ Ref.read state.transactionDictionaryRef
              updatedTransactionDictionary <- liftAff $ runStateT (TransactionStore.Memory.eval $ TransactionStore.put hash txSum) transactionDictionary
              liftEffect $ Ref.write (snd updatedTransactionDictionary) state.transactionDictionaryRef
              sendTxTxSum { status: statusToString Ok
                          , hash: hash
                          , hex: txHex
                          , decoded: txSum
                          }

type TxPostRequestBody = { tx :: HexStr }

sendTxTxSum :: Tx TxSum -> Handler
sendTxTxSum = sendJson <<< encodeTxWith encodeTxSum

--------------------------------------------------------------------------------

-- TODO add "data" field modeled after the `StateboxException` code in the js codebase
type TxErrorResponseBody =
  { status  :: String
  , code    :: String
  , message :: String
  }

toTxErrorResponseBody :: TxError -> TxErrorResponseBody
toTxErrorResponseBody err =
  { status  : statusCode Failed
  , code    : txErrorCode err
  , message : txErrorMessage err
  }

sendTxError :: TxError -> Handler
sendTxError = sendJson <<< toTxErrorResponseBody

--------------------------------------------------------------------------------


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
data Status = Ok | Failed

statusCode :: Status -> String
statusCode = case _ of
  Ok     -> "ok"
  Failed -> "failed"


instance showStatus :: Show Status where
  show = statusCode

statusToString :: Status -> String
statusToString = show -- TODO this should be a JSON-compatible value; perhaps a regular JSON encoder
