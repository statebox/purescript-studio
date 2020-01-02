module Statebox.Client where

import Prelude
import Affjax as Affjax
import Affjax (URL, Response)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseFormat (ResponseFormatError, printResponseFormatError)
import Affjax.RequestBody as RequestBody
import Control.Alt ((<|>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, close, produceAff, Emitter)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Profunctor.Choice ((|||))
import Data.HTTP.Method (Method(GET, POST))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashTx, TxId, TxSum(..), evalTxSum, isUberRootHash, attachTxId)
import Statebox.Core.Transaction.Codec (decodeTxTxSum)
import Statebox.Core.Types (HexStr)
import Statebox.Service.Codec (decodeTxErrorResponseBody)


newtype DecodingError = DecodingError String

instance eqDecodingError :: Eq DecodingError where
  eq (DecodingError x) (DecodingError y) = x == y

instance showDecodingError :: Show DecodingError where
  show = case _ of
    DecodingError e -> "(DecodingError " <> show e <> ")"

newtype TxNotFoundError = TxNotFoundError String

instance showTxNotFoundError :: Show TxNotFoundError where
  show (TxNotFoundError s) = "TxNotFoundError: " <> s

data ClientError
  = ClientResponseFormatError ResponseFormatError
  | ClientDecodingError       DecodingError
  | ClientTxNotFoundError     TxNotFoundError

instance showClientError :: Show ClientError where
  show (ClientResponseFormatError responseFormatError) = "Response format error: "       <> printResponseFormatError responseFormatError
  show (ClientDecodingError       decodingError      ) = "Decoding error: "              <> show decodingError
  show (ClientTxNotFoundError     txNotFoundError    ) = "Transaction not found error: " <> show txNotFoundError

evalClientError
  :: forall a
  .  (ResponseFormatError -> a)
  -> (DecodingError       -> a)
  -> (TxNotFoundError     -> a)
  -> ClientError
  -> a
evalClientError onResponseFormatError onDecodingError onTxNotFoundError clientError =
  case clientError of
    ClientResponseFormatError responseFormatError -> onResponseFormatError responseFormatError
    ClientDecodingError       decodingError       -> onDecodingError       decodingError
    ClientTxNotFoundError     txNotFoundError     -> onTxNotFoundError     txNotFoundError

-- | A convenience function for processing API responses.
evalTransactionResponse
  :: forall a
   . (ResponseFormatError -> a)
  -> (DecodingError       -> a)
  -> (TxNotFoundError     -> a)
  -> (HashTx              -> a)
  -> ClientError \/ HashTx
  -> a
evalTransactionResponse onResponseFormatError onDecodingError onTxNotFoundError onTx =
  evalClientError onResponseFormatError onDecodingError onTxNotFoundError ||| onTx

--------------------------------------------------------------------------------

-- | Request a single transaction from the API.
requestTransaction :: URL -> TxId -> Aff (ClientError \/ HashTx)
requestTransaction apiBaseUrl hash =
   requestTransaction' apiBaseUrl hash # map (map (attachTxId hash))

parseTxTxSum :: Json -> String \/ (ClientError \/ TxSum)
parseTxTxSum = (map (Right <<< _.decoded)) <<< decodeTxTxSum

parseTxNotFoundError :: Json -> String \/ (ClientError \/ TxSum)
parseTxNotFoundError = (map (Left <<< ClientTxNotFoundError <<< TxNotFoundError <<< _.message)) <<< decodeTxErrorResponseBody

fromEither :: forall a b . (a -> b) -> (Either a b) -> b
fromEither f = either f identity

-- TODO: handle empty string as TxId or exlude the possibility at the type level
requestTransaction' :: URL -> TxId -> Aff (ClientError \/ TxSum)
requestTransaction' apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    let
      tx :: ClientError \/ TxSum
      tx = do
        json <- lmap ClientResponseFormatError res.body
        fromEither (Left <<< ClientDecodingError <<< DecodingError) $ parseTxTxSum json <|> parseTxNotFoundError json
    pure tx

--------------------------------------------------------------------------------

requestTransactionJson :: URL -> TxId -> Aff (Response (ResponseFormatError \/ Json))
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

postTransactionHex :: URL -> HexStr -> Aff (Response (ResponseFormatError \/ Json))
postTransactionHex apiBaseUrl txHex =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx"
                                         , method = Left POST
                                         , responseFormat = ResponseFormat.json
                                         , content = pure $ RequestBody.json $ encodeJson {tx: txHex}
                                         }
