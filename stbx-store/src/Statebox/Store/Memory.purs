-- | In-memory storage.
module Statebox.Store.Memory where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, modify_)
import Data.Map (insert, lookup) as Map
import Data.Map (Map)

import Statebox.Store.Types (ActionF(Get, Put), Actions)

-- | Interpret the given actions as state updates to a dictionary.
eval :: forall k v a m. Ord k => MonadRec m => MonadState (Map k v) m => Actions k v a -> m a
eval = runFreeM \action -> case action of
  Get key next -> do
    valuesMap <- get
    pure $ next $ Map.lookup key valuesMap
  Put key value next -> do
    modify_ $ Map.insert key value
    pure next
