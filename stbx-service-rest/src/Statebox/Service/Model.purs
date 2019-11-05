module Statebox.Service.Model where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, modify_)
import Data.Argonaut.Core (Json, fromObject)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (insert, lookup) as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object, empty, insert)

import Statebox.Core.Transaction (HashTx, TxSum, TxId)
import Statebox.Core.Transaction.Codec (encodeTxSum)

-- | A DSL of operations on the transaction data store. We can make these
-- | abstract operations concrete by implementing them for a specific store, such
-- | as Postgres, Firestore, or in-memory. Alternatively, we could process them
-- | in some other way, such as logging them.
data ActionF a
  = GetTransaction String (Maybe TransactionDictionaryValue -> a)
  | PutTransaction HashTx a

derive instance functorActionF :: Functor (ActionF)

type Actions = Free ActionF


--------------------------------------------------------------------------------
-- Convenience injections.
--------------------------------------------------------------------------------

getTransaction :: String -> Actions (Maybe TransactionDictionaryValue)
getTransaction txHash = liftF $ GetTransaction txHash identity

putTransaction :: TxId -> TransactionDictionaryValue -> Actions Unit
putTransaction id tx = liftF $ PutTransaction {id, tx} unit


--------------------------------------------------------------------------------
-- Interpreter for logging the transaction storage actions.
--------------------------------------------------------------------------------

loggingActions :: forall a m. MonadEffect m => MonadRec m => Actions a -> m a
loggingActions = runFreeM $ \action -> case action of
  GetTransaction txHash next      -> do
    log $ "get transaction " <> txHash
    pure $ next Nothing
  PutTransaction transaction next -> do
    log $ "put transaction" <> show transaction
    pure next

--------------------------------------------------------------------------------
-- Interpreter for in-memory transaction storage.
--------------------------------------------------------------------------------

-- TODO #237 Discuss whether this should be `TxSum` or `Tx TxSum`, then eliminate this alias.
type TransactionDictionaryValue = TxSum

type TransactionDictionary = Map TxId TransactionDictionaryValue

encodeTransactionDictionary :: TransactionDictionary -> Json
encodeTransactionDictionary = fromObject <<< (foldrWithIndex addIndex empty)
  where
    addIndex :: TxId -> TxSum -> Object Json -> Object Json
    addIndex id transaction = insert id (encodeTxSum transaction)

-- | Interpret the given actions as state updates to a transaction dictionary.
inMemoryActions :: forall a m. MonadRec m => MonadState TransactionDictionary m => Actions a -> m a
inMemoryActions = runFreeM \action -> case action of
  GetTransaction txHash next -> do
    transactionsMap <- get
    pure $ next $ Map.lookup txHash transactionsMap
  PutTransaction { id, tx } next -> do
    modify_ $ Map.insert id tx
    pure next
