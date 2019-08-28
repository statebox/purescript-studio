module Statebox.Service.Model where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, modify_)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Statebox.Core.Transaction (Tx, TxSum, TxId)

-- define possible actions

type AddTransactionData =
  { hash        :: String
  , transaction :: TransactionDictionaryValue
  }

data ActionsF a
  = GetTransaction String (Maybe TransactionDictionaryValue -> a)
  | AddTransaction AddTransactionData a

derive instance functorActions :: Functor (ActionsF)

type Actions = Free ActionsF

-- define basic interactions

getTransaction :: String -> Actions (Maybe TransactionDictionaryValue)
getTransaction txHash = liftF $ GetTransaction txHash identity

putTransaction :: String -> TransactionDictionaryValue -> Actions Unit
putTransaction hash transaction = liftF $ AddTransaction {hash: hash, transaction: transaction} unit

-- logging instance

loggingActions :: forall a m. MonadEffect m => MonadRec m => Actions a -> m a
loggingActions = runFreeM $ \action -> case action of
  GetTransaction txHash next      -> do
    log $ "get transaction " <> txHash
    pure $ next Nothing
  AddTransaction transaction next -> do
    log $ "put transaction" <> show transaction
    pure next

-- in-memory instance

--------------------------------------------------------------------------------

-- TODO qua data we need, (Tx TxSum) is more correct (can we deserialize that?)
-- TODO qua data we need, (Tx TxSum) is more correct (can we deserialize that?)
-- TODO qua data we need, (Tx TxSum) is more correct (can we deserialize that?)
-- type TransactionDictionaryValue = Tx TxSum
-- TODO #237 Discuss whether this should be `TxSum` or `Tx TxSum`, then eliminate this alias.
type TransactionDictionaryValue = TxSum

-- type TransactionDictionary = Map TxId (Tx TxSum)
-- type TransactionDictionary = Map TxId TxSum
type TransactionDictionary = Map TxId TransactionDictionaryValue

inMemoryActions :: forall a m. MonadRec m => MonadState TransactionDictionary m => Actions a -> m a
inMemoryActions = runFreeM $ \action -> case action of
  GetTransaction txHash next -> do
    transactionsMap <- get
    pure $ next $ lookup txHash transactionsMap
    -- TODO this hash, transaction thing is iso to the existing type HashTx
    -- TODO this hash, transaction thing is iso to the existing type HashTx
    -- TODO this hash, transaction thing is iso to the existing type HashTx
  AddTransaction { hash, transaction } next -> do
    modify_ $ insert hash transaction
    pure next
