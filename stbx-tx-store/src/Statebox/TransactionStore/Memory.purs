-- | In-memory transaction storage.
module Statebox.TransactionStore.Memory where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, modify_)
import Data.Map (insert, lookup) as Map
import Data.Map (Map)

import Statebox.TransactionStore.Types (ActionF(Get, Put), Actions)

-- | Interpret the given actions as state updates to a transaction dictionary.
eval :: forall k v a m. Ord k => MonadRec m => MonadState (Map k v) m => Actions k v a -> m a
eval = runFreeM \action -> case action of
  Get txHash next -> do
    transactionsMap <- get
    pure $ next $ Map.lookup txHash transactionsMap
  Put id tx next -> do
    modify_ $ Map.insert id tx
    pure next
