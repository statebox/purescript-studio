module Statebox.Protocol.Store.Handler where

import Prelude
import Control.Monad.Free (Free, hoistFree, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Trans (StateT(..))
import Data.Map (Map)
import Data.Tuple.Nested (type (/\), (/\))

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

class Embeddable m' m where
  embed :: forall a. m' a -> m a

instance embeddableTxSum
  :: Functor m
  => Embeddable (StateT (Map String TxSum     ) m)
                (StateT (Map String TxSum /\ e) m)
  where
    embed (StateT f) = StateT (\(mapTxSum /\ e) -> (((\m -> m /\ e) <$> _) <$> _) $ f mapTxSum)

instance embeddableExecutionState
  :: Functor m
  => Embeddable (StateT (     Map String ExecutionState) m)
                (StateT (e /\ Map String ExecutionState) m)
 where
    embed (StateT f) = StateT (\(e /\ mapTxSum) -> (((\m -> e /\ m) <$> _) <$> _) $ f mapTxSum)

evalMultipleStoresActions
  :: forall a m m' m''. MonadRec m
  => Embeddable m'  m
  => Embeddable m'' m
  => (forall b. Actions TxId TxSum b          -> m' b)
  -> (forall c. Actions TxId ExecutionState c -> m'' c)
  -> MultipleStoresActions a                  -> m a
evalMultipleStoresActions evalTransactions evalExecutionStates = runFreeM $ case _ of
  Transaction    transactionActions    -> embed $ evalTransactions    transactionActions
  ExecutionState executionStateActions -> embed $ evalExecutionStates executionStateActions

eval
  :: forall a m m' m''. MonadRec m
  => Embeddable m'  m
  => Embeddable m'' m
  => (forall b. Actions TxId TxSum b          -> m' b)
  -> (forall c. Actions TxId ExecutionState c -> m'' c)
  -> StoreActions a                           -> m a
eval evalTransactions evalExecutionStates = hoistToMultipleStores >>> evalMultipleStoresActions evalTransactions evalExecutionStates
