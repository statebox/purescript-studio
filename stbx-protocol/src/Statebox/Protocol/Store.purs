module Statebox.Protocol.Store where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)

import Statebox.Core.Transaction (TxSum, TxId)
import Statebox.Protocol.Execution (Execution)

data StoreActionF k tv ev a
  = GetTransaction       k (Maybe tv -> a)
  | PutTransaction       k tv a
  | GetExecutionState    k (Maybe ev -> a)
  | UpdateExecutionState k ev a

derive instance functorStoreActionF :: Functor (StoreActionF k tv ev)

type StoreActions = Free (StoreActionF TxId TxSum Execution)

getTransaction :: TxId -> StoreActions (Maybe TxSum)
getTransaction hash = liftF $ GetTransaction hash identity

putTransaction :: TxId -> TxSum -> StoreActions Unit
putTransaction hash txSum = liftF $ PutTransaction hash txSum unit

getExecutionState :: TxId -> StoreActions (Maybe Execution)
getExecutionState hash = liftF $ GetExecutionState hash identity

updateExecutionState :: TxId -> Execution -> StoreActions Unit
updateExecutionState hash execution = liftF $ UpdateExecutionState hash execution unit
