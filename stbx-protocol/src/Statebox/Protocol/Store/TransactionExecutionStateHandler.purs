module Statebox.Protocol.Store.TransactionExecutionStateHandler where

import Prelude
import Control.Monad.Free (Free, hoistFree, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Trans (StateT(..))
import Data.Map (Map)
import Data.Tuple.Nested (type (/\), (/\))

import Statebox.Core.Transaction (TxSum, TxId)
import Statebox.Protocol.ExecutionState (ExecutionState)
import Statebox.Protocol.Store (StoreActions, StoreActionF(..))
import Statebox.Store (Actions, get, put) as Store

data MultipleStoresActionF a
  = Transaction    (Store.Actions TxId TxSum          a)
  | ExecutionState (Store.Actions TxId ExecutionState a)

derive instance multipleStoresActionFunctor :: Functor MultipleStoresActionF

type MultipleStoresActions = Free MultipleStoresActionF

hoistToMultipleStores :: ∀ a. StoreActions a -> MultipleStoresActions a
hoistToMultipleStores = hoistFree (case _ of
  GetTransaction       key       next -> Transaction    (next <$> Store.get key)
  PutTransaction       key value next -> Transaction    (next <$  Store.put key value)
  GetExecutionState    key       next -> ExecutionState (next <$> Store.get key)
  UpdateExecutionState key value next -> ExecutionState (next <$  Store.put key value))

-- | This typeclass describes a natural transformation between `ma` and `m` (if they are functors).
-- | It allows us to transform naturally instances of `ma` to instances of `m`
class Embeddable ma m where
  embed :: ∀ a. ma a -> m a

instance embeddableTxSum
  :: Functor m
  => Embeddable (StateT (Map String TxSum     ) m)
                (StateT (Map String TxSum /\ e) m)
  where
    embed (StateT f) = StateT (\(transactionDictionary /\ e) -> (((\m -> m /\ e) <$> _) <$> _) $ f transactionDictionary)

instance embeddableExecutionState
  :: Functor m
  => Embeddable (StateT (     Map String ExecutionState) m)
                (StateT (t /\ Map String ExecutionState) m)
 where
    embed (StateT f) = StateT (\(t /\ executionStateDictionary) -> (((\m -> t /\ m) <$> _) <$> _) $ f executionStateDictionary)

evalMultipleStoresActions
  :: ∀ m mb mc a
   . MonadRec      m
  => Embeddable mb m
  => Embeddable mc m
  => (∀ b. Store.Actions TxId TxSum          b -> mb b)
  -> (∀ c. Store.Actions TxId ExecutionState c -> mc c)
  -> MultipleStoresActions                   a -> m  a
evalMultipleStoresActions evalTransactions evalExecutionStates = runFreeM case _ of
  Transaction    transactionActions    -> embed $ evalTransactions    transactionActions
  ExecutionState executionStateActions -> embed $ evalExecutionStates executionStateActions

eval
  :: ∀ m mb mc a
   . MonadRec      m
  => Embeddable mb m
  => Embeddable mc m
  => (∀ b. Store.Actions TxId TxSum          b -> mb b)
  -> (∀ c. Store.Actions TxId ExecutionState c -> mc c)
  -> StoreActions                            a -> m  a
eval evalTransactions evalExecutionStates = hoistToMultipleStores >>> evalMultipleStoresActions evalTransactions evalExecutionStates
