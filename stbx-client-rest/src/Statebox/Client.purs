module Statebox.Client where

import Prelude
import Affjax as Affjax
import Affjax (URL, Response)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, close, produceAff, Emitter)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Profunctor.Choice ((|||), (+++))
import Data.HTTP.Method (Method(GET, POST))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashTx, TxId, TxSum(..), evalTxSum, isUberRootHash, attachTxId)
import Statebox.Core.Transaction.Codec (decodeTxTxSum, DecodingError(..))
import Statebox.Core.Types (HexStr)


-- | A convenience function for processing API responses.
evalTransactionResponse
  :: forall a
   . (Affjax.Error  -> a)
  -> (DecodingError -> a)
  -> (HashTx        -> a)
  -> Affjax.Error \/ (DecodingError \/ HashTx)
  -> a
evalTransactionResponse onAffjaxError onDecodingError onTx =
  onAffjaxError ||| onDecodingError ||| onTx

--------------------------------------------------------------------------------

-- | Request a single transaction from the API and return it along with its hash.
requestTransaction :: URL -> TxId -> Aff (Affjax.Error \/ (DecodingError \/ HashTx))
requestTransaction apiBaseUrl hash =
   requestTransaction' apiBaseUrl hash # map (map (map (attachTxId hash)))

-- | Request a single transaction from the API and return it along with its hash.
requestTransaction' :: URL -> TxId -> Aff (Affjax.Error \/ (DecodingError \/ TxSum))
requestTransaction' apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right <<< Right $ UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    let
      tx :: Affjax.Error \/ DecodingError \/ TxSum
      tx = (DecodingError +++ _.decoded) <<< decodeTxTxSum <<< _.body <$> res
    pure tx

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

postTransactionHex :: URL -> HexStr -> Aff (Affjax.Error \/ Response Json)
postTransactionHex apiBaseUrl txHex =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx"
                                         , method = Left POST
                                         , responseFormat = ResponseFormat.json
                                         , content = pure $ RequestBody.json $ encodeJson {tx: txHex}
                                         }
