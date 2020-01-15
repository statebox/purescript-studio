module Statebox.Client where

import Prelude
import Affjax as Affjax
import Affjax (URL, Response)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Control.Alt ((<|>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, close, produceAff, Emitter)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Profunctor.Choice ((|||))
import Data.HTTP.Method (Method(GET, POST))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashTx, TxId, TxSum(..), evalTxSum, isUberRootHash, attachTxId)
import Statebox.Core.Transaction.Codec (decodeTxTxSum)
import Statebox.Core.Types (HexStr)
import Statebox.Service (TxError, decodeTxError)
import Statebox.Service.Codec (decodeTxErrorResponseBody)


-- we use this name instead of `DecodingError` to align with the function name `jsonDecode`
newtype JsonDecodeError = JsonDecodeError String

instance eqJsonDecodeError :: Eq JsonDecodeError where
  eq (JsonDecodeError x) (JsonDecodeError y) = x == y

instance showJsonDecodeError :: Show JsonDecodeError where
  show = case _ of
    JsonDecodeError e -> "(JsonDecodeError " <> show e <> ")"

data ClientError
  = ClientAffjaxError     Affjax.Error
  | ClientJsonDecodeError JsonDecodeError
  | ClientTxError         TxError

instance showClientError :: Show ClientError where
  show (ClientAffjaxError     affjaxError   ) = "Response format error: " <> Affjax.printError affjaxError
  show (ClientJsonDecodeError decodingError ) = "Decoding error: "        <> show decodingError
  show (ClientTxError         txError       ) = "Transaction error: "     <> show txError

evalClientError
  :: forall a
  .  (Affjax.Error    -> a)
  -> (JsonDecodeError -> a)
  -> (TxError         -> a)
  -> ClientError
  -> a
evalClientError onAffjaxError onJsonDecodeError onTxError clientError =
  case clientError of
    ClientAffjaxError     affjaxError   -> onAffjaxError     affjaxError
    ClientJsonDecodeError decodingError -> onJsonDecodeError decodingError
    ClientTxError         txError       -> onTxError         txError

-- | A convenience function for processing API responses.
evalTransactionResponse
  :: forall a
   . (Affjax.Error    -> a)
  -> (JsonDecodeError -> a)
  -> (TxError         -> a)
  -> (HashTx          -> a)
  -> ClientError \/ HashTx
  -> a
evalTransactionResponse onAffjaxError onJsonDecodeError onTxError onTx =
  evalClientError onAffjaxError onJsonDecodeError onTxError ||| onTx

--------------------------------------------------------------------------------

-- | Request a single transaction from the API.
requestTransaction :: URL -> TxId -> Aff (ClientError \/ HashTx)
requestTransaction apiBaseUrl hash =
   requestTransaction' apiBaseUrl hash # map (map (attachTxId hash))

parseTxTxSum :: Json -> String \/ (ClientError \/ TxSum)
parseTxTxSum = (map (Right <<< _.decoded)) <<< decodeTxTxSum

parseTxError :: forall a . Json -> String \/ (ClientError \/ a)
parseTxError = decodeTxErrorResponseBody >>>
  (map ((either (Left <<< ClientJsonDecodeError <<< JsonDecodeError) (Left <<< ClientTxError)) <<< decodeTxError <<< _.error))

fromEither :: forall a b . (a -> b) -> (Either a b) -> b
fromEither f = either f identity

processResponse :: Affjax.Error \/ Response Json -> ClientError \/ TxSum
processResponse
  =   Left <<< ClientAffjaxError
  ||| (\response ->
    let json = response.body in
    fromEither (Left <<< ClientJsonDecodeError <<< JsonDecodeError) $ parseTxTxSum json <|> parseTxError json)

-- TODO: handle empty string as TxId or exclude the possibility at the type level
requestTransaction' :: URL -> TxId -> Aff (ClientError \/ TxSum)
requestTransaction' apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    pure $ processResponse res

--------------------------------------------------------------------------------

requestTransactionJson :: URL -> TxId -> Aff (Affjax.Error \/ Response Json)
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = txUrl apiBaseUrl hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

-- TODO consider defining a 1-ary helper that does this on WiringFiringInfo / NamespaceInfo / other
txUrl :: URL -> TxId -> URL
txUrl apiBaseUrl txId = apiBaseUrl <> "/tx/" <> txId

--------------------------------------------------------------------------------

-- | Request the transaction corresponding to the specified `startHash`, as well as all of its
-- | parent transactions up to the root.
-- |
-- | The transactions are loaded one by one and emitted (streamed) onto a `Producer` coroutine.
-- |
-- | This is a generalized version of `requestTransactionsToRoot`.
requestTransactionsToRootM :: forall m. MonadAff m => URL -> TxId -> Producer HashTx m Unit
requestTransactionsToRootM apiBaseUrl startHash =
  hoistFreeT liftAff $ requestTransactionsToRoot apiBaseUrl startHash

-- | Request the transaction corresponding to the specified `startHash`, as well as all of its
-- | parent transactions up to the root.
-- |
-- | The transactions are loaded one by one and emitted (streamed) onto a `Producer` coroutine.
-- |
-- | This is an `Aff`- specialized version of `requestTransactionsToRootM`. (In fact, the more
-- | general one is implemented in terms of this one.)
requestTransactionsToRoot :: URL -> TxId -> Producer HashTx Aff Unit
requestTransactionsToRoot apiBaseUrl startHash =
  produceAff $ \emitter -> tailRecM (fetchAndEmitTxStep emitter apiBaseUrl) startHash

fetchAndEmitTxStep :: Emitter Aff HashTx Unit -> URL -> TxId -> Aff (Step TxId Unit)
fetchAndEmitTxStep emitter apiBaseUrl hash = liftAff $ do
  requestTransaction apiBaseUrl hash >>= evalTransactionResponse
    (\e -> do close emitter unit -- TODO emit error?
              pure $ Done unit)
    (\e -> do close emitter unit -- TODO emit error?
              pure $ Done unit)
    (\e -> do close emitter unit -- TODO emit error?
              pure $ Done unit)
    (\{id, tx} -> evalTxSum
       (\x -> do close emitter unit
                 pure $ Done unit)
       (\x -> do emit emitter { id, tx }
                 pure $ Loop x.previous)
       (\x -> do emit emitter { id, tx }
                 pure $ Loop x.previous)
       (\x -> do emit emitter { id, tx }
                 pure $ Loop x.previous)
       tx
    )

--------------------------------------------------------------------------------

postTransactionHexJson :: URL -> HexStr -> Aff (Affjax.Error \/ Response Json)
postTransactionHexJson apiBaseUrl txHex =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx"
                                         , method = Left POST
                                         , responseFormat = ResponseFormat.json
                                         , content = pure $ RequestBody.json $ encodeJson {tx: txHex}
                                         }

postTransactionHex :: URL -> HexStr -> Aff (ClientError \/ TxSum)
postTransactionHex apiBaseUrl txHex = do
  res <- postTransactionHexJson apiBaseUrl txHex
  pure $ processResponse res

--------------------------------------------------------------------------------

evalPostTransaction
  :: forall a
  .  (Affjax.Error    -> a)
  -> (JsonDecodeError -> a)
  -> (TxError         -> a)
  -> (TxSum           -> a)
  -> ClientError \/ TxSum
  -> a
evalPostTransaction onAffjaxError onJsonDecodeError onTxError onTxSum =
  evalClientError onAffjaxError onJsonDecodeError onTxError ||| onTxSum
