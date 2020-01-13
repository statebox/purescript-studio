module Statebox.Protocol where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Statebox.Core.Transaction (HashStr, InitialTx, TxId, TxSum(..), WiringTx, evalTxSum, isUberRootHash)
import Statebox.TransactionStore.Types (Actions, get, put)

data ProcessError
  = NoUberRoot
  | InitialPreviousNoUberRoot TxId
  | WiringNotPreviousInitial  TxId

processTxSum :: HashStr -> TxSum -> Actions (Either ProcessError Unit)
processTxSum hash = case _ of
  UberRootTxInj           -> pure $ Left NoUberRoot
  InitialTxInj  initialTx -> processInitialTx hash initialTx
  WiringTxInj   wiringTx  -> processWiringTx  hash wiringTx
  FiringTxInj   firingTx  -> ?f

processInitialTx :: HashStr -> InitialTx -> Actions (Either ProcessError Unit)
processInitialTx hash initialTx =
  if (isUberRootHash initialTx.previous)
  then map Right $ put hash $ InitialTxInj initialTx
  else pure $ Left $ InitialPreviousNoUberRoot initialTx.previous

isInitialTx :: TxId -> Actions Boolean
isInitialTx hash = do
  maybeTxSum <- get hash
  case maybeTxSum of
    Nothing -> pure false
    Just tx -> pure $ evalTxSum (const false) (const true) (const false) (const false) tx

processWiringTx :: HashStr -> WiringTx -> Actions (Either ProcessError Unit)
processWiringTx hash wiringTx =
  let
    previousHash = wiringTx.previous
  in do
    isPreviousInitial <- isInitialTx previousHash
    if isPreviousInitial
    then map Right $ put hash $ WiringTxInj wiringTx
    else pure $ Left $ WiringNotPreviousInitial hash
