module Statebox.Service.Main where

import Prelude

import Control.Monad.State.Trans (StateT, runStateT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (fromObject) as Json
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function.Uncurried (Fn3)
import Data.Lens (Lens', view, _1, _2)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (new, read, write) as Ref
import Effect.Ref (Ref)
import Effect.Exception (Error, error, message)
import Foreign.Object (Object, empty, insert)
import Node.Express.App (App, listenHttp, get, post, use, useExternal, useOnError)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getBody', getRouteParam, getOriginalUrl)
import Node.Express.Response (sendJson, setStatus, setResponseHeader)
import Node.Express.Types (Request, Response)
import Node.HTTP (Server)
import Unsafe.Coerce (unsafeCoerce)

import Statebox.Core (hash) as Stbx
import Statebox.Core.Transaction (TxId, Tx, TxSum(..), uberRootHash)
import Statebox.Core.Transaction.Codec (encodeTxWith, encodeTxSum)
import Statebox.Protocol (ProcessError, processTxSum)
import Statebox.Protocol.ExecutionState (ExecutionState)
import Statebox.Protocol.Store.TransactionExecutionStateHandler (eval) as Store
import Statebox.Service.Codec (parseBodyToJson, jsonBodyToTxString, txStringToTxJsonString', txJsonStringToTxData', txDataToTxSum')
import Statebox.Service.Error (ResponseError, TxError(..), processErrorToTxError, responseErrorToTxError, toTxErrorResponseBody)
import Statebox.Service.Status (Status(..))
import Statebox.Store (get) as Single.Store
import Statebox.Store.Memory (eval) as Store.Memory
import Statebox.Store.Types (Actions) as Store

foreign import stringBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

stbxPort :: Int
stbxPort = 8008

-- application state

-- TODO #237 Discuss whether this should be `TxSum` or `Tx TxSum`, then eliminate this alias.
type TransactionDictionaryValue = TxSum

type TransactionDictionary = Map TxId TransactionDictionaryValue

encodeTransactionDictionary :: TransactionDictionary -> Json
encodeTransactionDictionary = Json.fromObject <<< foldrWithIndex addIndex empty
  where
    addIndex :: TxId -> TxSum -> Object Json -> Object Json
    addIndex id transaction = insert id (encodeTxSum transaction)

type ExecutionStateDictionaryValue = ExecutionState

type ExecutionStateDictionary = Map TxId ExecutionStateDictionaryValue

type AppState = Ref TransactionDictionary /\ Ref ExecutionStateDictionary

_transactionDictionaryRef :: Lens' AppState (Ref TransactionDictionary)
_transactionDictionaryRef = _1

_executionStateDictionaryRef :: Lens' AppState (Ref ExecutionStateDictionary)
_executionStateDictionaryRef = _2

initialState :: Effect AppState
initialState =
  (/\) <$> Ref.new initialTransactionDictionary
       <*> Ref.new initialExecutionStateDictionary
  where
    initialTransactionDictionary = Map.singleton uberRootHash UberRootTxInj
    initialExecutionStateDictionary = mempty

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
  , endpoints   : { ping            : "/tx/ping"
                  , getTransactions : "/tx"
                  , getTransaction  : "/tx/:hash"
                  }
  }

ping :: Handler
ping = sendJson { health: "I'm fine" }

-- | Endpoint for getting all the transactions from the transaction store.
-- | Responds to `GET /tx`
getTransactionsHandler :: AppState -> Handler
getTransactionsHandler state = do
  setResponseHeader "Access-Control-Allow-Origin" "*"
  transactionDictionary <- liftEffect $ Ref.read (view _transactionDictionaryRef state)
  sendJson { status: show Ok
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
      transactionDictionary <- liftEffect $ Ref.read (view _transactionDictionaryRef state)
      maybeTransaction <- liftAff $ runStateT (Store.Memory.eval $ Single.Store.get hash) transactionDictionary
      case maybeTransaction of
        Just transaction /\ _ -> sendTxTxSum
          { status: show Ok
          , hash: hash
          , hex: "TODO" -- TODO #237 get from transaction store instead of computing, if possible
          , decoded: transaction
          }
        Nothing /\ _ -> sendTxError (TxNotFound { hash })



-- TODO leuk met +++ ofzo?

-- | Endpoint for saving transactions to the transaction store.
-- | Responds to `POST /tx`.
postTransactionHandler :: AppState -> Handler
postTransactionHandler state = do
  -- TODO: find a proper way to manage body decoding
  bodyStr :: String <- unsafeCoerce <$> getBody'
  parseTxSum bodyStr # either (sendTxError <<< responseErrorToTxError)
                              storeAndSendTx
  where
    parseTxSum :: String -> ResponseError \/ (TxId /\ TxSum)
    parseTxSum = parseBodyToJson         -- convert body from string to json
             >=> jsonBodyToTxString      -- retrieve hex string from tx field of body
             >=> txStringToTxJsonString' -- decode hex string to string using js service
             >=> txJsonStringToTxData'   -- parse string to json
             >=> txDataToTxSum'          -- parse json into txSum

    storeAndSendTx :: TxId /\ TxSum -> Handler
    storeAndSendTx (txHex /\ txSum) = do
      let hash = Stbx.hash txHex
      transactionDictionary    <- liftEffect $ Ref.read (view _transactionDictionaryRef    state)
      executionStateDictionary <- liftEffect $ Ref.read (view _executionStateDictionaryRef state)

      (processingResult :: ProcessError \/ Unit) /\ (transactionDictionaryUpdated /\ executionStateDictionaryUpdated) <- liftAff $
        runStateT (Store.eval (Store.Memory.eval :: ∀ b. Store.Actions TxId TxSum          b -> StateT TransactionDictionary    Aff b)
                              (Store.Memory.eval :: ∀ c. Store.Actions TxId ExecutionState c -> StateT ExecutionStateDictionary Aff c)
                              (processTxSum { id: hash, tx: txSum }))
                  (transactionDictionary /\ executionStateDictionary)

      processingResult # either
        (sendTxError <<< processErrorToTxError)
        (const do liftEffect $ Ref.write transactionDictionaryUpdated    (_transactionDictionaryRef    `view` state)
                  liftEffect $ Ref.write executionStateDictionaryUpdated (_executionStateDictionaryRef `view` state)
                  sendTxTxSum { status: show Ok
                              , hash: hash
                              , hex: txHex
                              , decoded: txSum
                              })

sendTxTxSum :: Tx TxSum -> Handler
sendTxTxSum = sendJson <<< encodeTxWith encodeTxSum

--------------------------------------------------------------------------------

sendTxError :: TxError -> Handler
sendTxError = sendJson <<< toTxErrorResponseBody

--------------------------------------------------------------------------------


-- application definition with routing

app :: AppState -> App
app state = do
  useExternal         stringBodyParser
  use                 logger
  get  "/"            index
  get  "/tx/ping"     ping
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
