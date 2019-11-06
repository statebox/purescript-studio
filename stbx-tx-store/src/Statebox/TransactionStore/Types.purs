module Statebox.TransactionStore.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)

import Statebox.Core.Transaction (TxSum, TxId)

-- | Operations on a key/value store.
data ActionF k v a
  = Get k (Maybe v -> a)
  | Put k v a

derive instance functorActionF :: Functor (ActionF k v)

-- | Our algebra of `ActionF`-operations on a key/value store. We can make the
-- | abstract operations concrete by implementing them for a specific store,
-- | such as Postgres, Firestore, or in-memory. Alternatively, we could process
-- | them in some other way, such as by logging them.
type Actions = Free (ActionF TxId TransactionDictionaryValue)

-- TODO #237 Discuss whether this should be `TxSum` or `Tx TxSum`, then eliminate this alias.
type TransactionDictionaryValue = TxSum

--------------------------------------------------------------------------------

getTransaction :: TxId -> Actions (Maybe TransactionDictionaryValue)
getTransaction txHash = liftF $ Get txHash identity

putTransaction :: TxId -> TransactionDictionaryValue -> Actions Unit
putTransaction id tx = liftF $ Put id tx unit
