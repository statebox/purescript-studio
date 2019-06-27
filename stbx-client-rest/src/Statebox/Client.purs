module Statebox.Client where

import Prelude
import Affjax as Affjax
import Affjax (URL, Response)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.RequestHeader as RequestHeader
import Affjax.StatusCode (StatusCode(..))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, close, produceAff, Emitter)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.HTTP.Method (Method(GET))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashTx, TxId, TxSum(..), evalTxSum, isUberRootHash, attachTxId)
import Statebox.Core.Transaction.Codec (decodeTxSum, DecodingError)

-- | A convenience function for processing API responses.
evalTransactionResponse
  :: forall a
   . (ResponseFormatError -> a)
  -> (DecodingError       -> a)
  -> (HashTx              -> a)
  -> ResponseFormatError \/ (DecodingError \/ HashTx)
  -> a
evalTransactionResponse onResponseFormatError onDecodingError onTx =
  (onResponseFormatError `either` (onDecodingError `either` onTx))

--------------------------------------------------------------------------------

-- | Request a single transaction from the API.
requestTransaction :: URL -> TxId -> Aff (ResponseFormatError \/ (DecodingError \/ HashTx))
requestTransaction apiBaseUrl hash =
   requestTransaction' apiBaseUrl hash # map (map (map (attachTxId hash)))

requestTransaction' :: URL -> TxId -> Aff (ResponseFormatError \/ (DecodingError \/ TxSum))
requestTransaction' apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right <<< Right $ UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    pure $ decodeTxSum <$> res.body

requestTransactionJson :: URL -> TxId -> Aff (Response (ResponseFormatError \/ Json))
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

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
    (\{hash: _, tx} -> evalTxSum
       (\x -> do close emitter unit
                 pure $ Done unit)
       (\x -> do emit emitter { hash, tx }
                 pure $ Loop x.previous)
       (\x -> do emit emitter { hash, tx }
                 pure $ Loop x.previous)
       (\x -> do emit emitter { hash, tx }
                 pure $ Loop x.previous)
       tx
    )
