module Statebox.TransactionStore.ConsoleLogger where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Statebox.TransactionStore.Types (ActionF(..), Actions)

-- | Interpreter transaction storage actions as console 'log's.
loggingActions :: forall a m. MonadEffect m => MonadRec m => Actions a -> m a
loggingActions = runFreeM $ \action -> case action of
  GetTransaction txHash next      -> do
    log $ "get transaction " <> txHash
    pure $ next Nothing
  PutTransaction transaction next -> do
    log $ "put transaction" <> show transaction
    pure next
