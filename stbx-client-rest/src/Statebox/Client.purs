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
import Control.Monad.Rec.Class (class MonadRec, Step(Loop, Done), tailRecM)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Statebox.Core.Transaction (HashStr, TxSum(..), evalTxSum, isUberRootHash)
import Statebox.Core.Transaction.Codec (decodeTxSum, DecodingError)

-- | A convenience function for processing API responses.
evalTransactionResponse
  :: forall a
   . (ResponseFormatError -> a)
  -> (DecodingError       -> a)
  -> (TxSum               -> a)
  -> ResponseFormatError \/ (DecodingError \/ TxSum)
  -> a
evalTransactionResponse onResponseFormatError onDecodingError onTx =
  (onResponseFormatError `either` (onDecodingError `either` onTx))

-- | Request a single transaction from the API.
requestTransaction :: URL -> HashStr -> Aff (ResponseFormatError \/ (DecodingError \/ TxSum))
requestTransaction apiBaseUrl hash =
  if isUberRootHash hash then
    pure $ Right <<< Right $ UberRootTxInj
  else do
    res <- requestTransactionJson apiBaseUrl hash
    pure $ decodeTxSum hash <$> res.body

-- | Request the transaction corresponding to the specified `startHash`, as well as all of its
-- | parent transactions up to the root.
-- |
-- | The transactions are loaded one by one and emitted (streamed) onto a `Producer` coroutine.
requestTransactionsToRoot :: URL -> HashStr -> Producer TxSum Aff Unit
requestTransactionsToRoot apiBaseUrl startHash =
  produceAff $ \emitter -> (tailRecM (fetchEmitStep emitter)) startHash
  where
    fetchEmitStep :: Emitter Aff TxSum Unit -> HashStr -> Aff (Step HashStr Unit)
    fetchEmitStep emitter hash = do
      requestTransaction apiBaseUrl hash >>= evalTransactionResponse
        (\e -> do close emitter unit -- TODO emit error?
                  pure $ Done unit)
        (\e -> do close emitter unit -- TODO emit error?
                  pure $ Done unit)
        (\tx -> evalTxSum
           (\x -> do close emitter unit
                     pure $ Done unit)
           (\x -> do emit emitter tx
                     pure $ Loop x.previous)
           (\x -> do emit emitter tx
                     pure $ Loop x.previous)
           (\x -> do emit emitter tx
                     pure $ Loop x.previous)
           tx
        )

requestTransactionJson :: URL -> HashStr -> Aff (Response (ResponseFormatError \/ Json))
requestTransactionJson apiBaseUrl hash =
  Affjax.request $ Affjax.defaultRequest { url = apiBaseUrl <> "/tx/" <> hash
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }
