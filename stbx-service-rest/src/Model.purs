module Model where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)

import Statebox.Core.Transaction (Tx, TxSum)

-- define possible actions

data ActionsF a
  = GetTransaction String (Maybe (Tx TxSum) -> a)
  | PutTransaction (Tx TxSum) a

derive instance functorActions :: Functor (ActionsF)

type Actions = Free ActionsF

-- define basic interactions

getTransaction :: String -> Actions (Maybe (Tx TxSum))
getTransaction txHash = liftF $ GetTransaction txHash identity

putTransaction :: Tx TxSum -> Actions Unit
putTransaction transaction = liftF $ PutTransaction transaction unit

-- loggingInstance

loggingActions :: forall a. Actions a -> Aff a
loggingActions = runFreeM $ \action -> case action of
  GetTransaction txHash next      -> do
    log $ "get transaction " <> txHash
    pure $ next Nothing
  PutTransaction transaction next -> do
    log $ "put transaction" <> show transaction
    pure next
