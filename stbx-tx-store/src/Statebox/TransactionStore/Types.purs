module Statebox.TransactionStore.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)

-- | Operations on a key/value store.
data ActionF k v a
  = Get k (Maybe v -> a)
  | Put k v a

derive instance functorActionF :: Functor (ActionF k v)

--------------------------------------------------------------------------------

-- | Our algebra of `ActionF`-operations on a key/value store. We can make the
-- | abstract operations concrete by implementing them for a specific store,
-- | such as Postgres, Firestore, or in-memory. Alternatively, we could process
-- | them in some other way, such as by logging them.
type Actions k v = Free (ActionF k v)

get :: forall k v. k -> Actions k v (Maybe v)
get txHash = liftF $ Get txHash identity

put :: forall k v. k -> v -> Actions k v Unit
put id tx = liftF $ Put id tx unit
