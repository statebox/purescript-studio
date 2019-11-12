module Statebox.TransactionStore.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)

import Statebox.Core.Transaction (HashTx, TxSum, TxId)

-- | Operations on the transaction data store. We can make these abstract
-- | operations concrete by implementing them for a specific store, such as
-- | Postgres, Firestore, or in-memory. Alternatively, we could process them in
-- | some other way, such as logging them.
data ActionF a
  = GetTransaction String (Maybe TransactionDictionaryValue -> a)
  | PutTransaction HashTx a

derive instance functorActionF :: Functor (ActionF)

type Actions = Free ActionF

-- TODO #237 Discuss whether this should be `TxSum` or `Tx TxSum`, then eliminate this alias.
type TransactionDictionaryValue = TxSum


--------------------------------------------------------------------------------
-- Convenience injections.
--------------------------------------------------------------------------------

getTransaction :: TxId -> Actions (Maybe TransactionDictionaryValue)
getTransaction txHash = liftF $ GetTransaction txHash identity

putTransaction :: TxId -> TransactionDictionaryValue -> Actions Unit
putTransaction id tx = liftF $ PutTransaction {id, tx} unit
