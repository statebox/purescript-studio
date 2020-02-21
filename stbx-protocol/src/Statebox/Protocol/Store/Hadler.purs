module Statebox.Protocol.Store.Handler where

import Prelude
import Control.Monad.Free (Free, hoistFree)

import Statebox.Core.Transaction (TxSum, TxId)
import Statebox.Protocol.ExecutionState (ExecutionState)
import Statebox.Protocol.Store (StoreActions, StoreActionF(..))
import Statebox.Store (Actions, get, put)

data MultipleStoresAction a
  = Transaction    (Actions TxId TxSum          a)
  | ExecutionState (Actions TxId ExecutionState a)

type MultipleStoresActions = Free MultipleStoresAction

eval :: forall a. StoreActions a -> MultipleStoresActions a
eval = hoistFree (case _ of
  GetTransaction       key       next -> Transaction (next <$> get key)
  PutTransaction       key value next -> Transaction (next <$ put key value)
  GetExecutionState    key       next -> ExecutionState (next <$> get key)
  UpdateExecutionState key value next -> ExecutionState (next <$ put key value))
