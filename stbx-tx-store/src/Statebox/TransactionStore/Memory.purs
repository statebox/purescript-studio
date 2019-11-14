-- | In-memory transaction storage.
module Statebox.TransactionStore.Memory where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, modify_)
import Data.Argonaut.Core (Json, fromObject)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (insert, lookup) as Map
import Data.Map (Map)
import Foreign.Object (Object, empty, insert)

import Statebox.Core.Transaction (TxSum, TxId)
import Statebox.Core.Transaction.Codec (encodeTxSum)
import Statebox.TransactionStore.Types (ActionF(Get, Put), Actions, TransactionDictionaryValue)


type TransactionDictionary = Map TxId TransactionDictionaryValue

-- | Interpret the given actions as state updates to a transaction dictionary.
eval :: forall a m. MonadRec m => MonadState TransactionDictionary m => Actions a -> m a
eval = runFreeM \action -> case action of
  Get txHash next -> do
    transactionsMap <- get
    pure $ next $ Map.lookup txHash transactionsMap
  Put id tx next -> do
    modify_ $ Map.insert id tx
    pure next

encodeTransactionDictionary :: TransactionDictionary -> Json
encodeTransactionDictionary = fromObject <<< (foldrWithIndex addIndex empty)
  where
    addIndex :: TxId -> TxSum -> Object Json -> Object Json
    addIndex id transaction = insert id (encodeTxSum transaction)
