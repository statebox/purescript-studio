module Statebox.Protocol.Store.Handler where

import Prelude
import Control.Monad.Free (Free, hoistFree, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)

import Statebox.Core.Transaction (TxSum, TxId)
import Statebox.Protocol.ExecutionState (ExecutionState)
import Statebox.Protocol.Store (StoreActions, StoreActionF(..))
import Statebox.Store (Actions, get, put)

data MultipleStoresActionF a
  = Transaction    (Actions TxId TxSum          a)
  | ExecutionState (Actions TxId ExecutionState a)

derive instance multipleStoresActionFunctor :: Functor MultipleStoresActionF

type MultipleStoresActions = Free MultipleStoresActionF

hoistToMultipleStores :: forall a. StoreActions a -> MultipleStoresActions a
hoistToMultipleStores = hoistFree (case _ of
  GetTransaction       key       next -> Transaction (next <$> get key)
  PutTransaction       key value next -> Transaction (next <$ put key value)
  GetExecutionState    key       next -> ExecutionState (next <$> get key)
  UpdateExecutionState key value next -> ExecutionState (next <$ put key value))

evalMultipleStoresActions
  :: forall a m. MonadRec m
  => (forall b. Actions TxId TxSum b          -> m b)
  -> (forall c. Actions TxId ExecutionState c -> m c)
  -> MultipleStoresActions a        -> m a
evalMultipleStoresActions evalTransactions evalExecutionStates = runFreeM $ case _ of
  Transaction    transactionActions    -> evalTransactions    transactionActions
  ExecutionState executionStateActions -> evalExecutionStates executionStateActions

eval
  :: forall a m. MonadRec m
  => (forall b. Actions TxId TxSum b          -> m b)
  -> (forall c. Actions TxId ExecutionState c -> m c)
  -> StoreActions a                           -> m a
eval evalTransactions evalExecutionStates = hoistToMultipleStores >>> evalMultipleStoresActions evalTransactions evalExecutionStates
