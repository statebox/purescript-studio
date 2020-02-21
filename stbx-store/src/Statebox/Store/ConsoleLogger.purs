module Statebox.Store.ConsoleLogger where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Statebox.Store.Types (ActionF(Get, Put), Actions)

-- | Interpreter storage actions as console 'log's.
eval :: forall k v a m. Show k => Show v => MonadEffect m => MonadRec m => Actions k v a -> m a
eval = runFreeM $ \action -> case action of
  Get key next -> do
    log $ "get key " <> show key
    pure $ next Nothing
  Put key value next -> do
    log $ "put value " <> show value <> " at key " <> show key
    pure next
