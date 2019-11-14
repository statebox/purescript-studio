module Statebox.TransactionStore.ConsoleLogger where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Statebox.TransactionStore.Types (ActionF(Get, Put), Actions)

-- | Interpreter transaction storage actions as console 'log's.
eval :: forall a m. MonadEffect m => MonadRec m => Actions a -> m a
eval = runFreeM $ \action -> case action of
  Get txHash next -> do
    log $ "get transaction " <> txHash
    pure $ next Nothing
  Put hash transaction next -> do
    log $ "put transaction " <> show transaction <> " at hash " <> hash
    pure next
