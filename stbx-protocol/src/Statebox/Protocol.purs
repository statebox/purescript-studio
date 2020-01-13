module Statebox.Protocol where

import Prelude
import Data.Either (Either(..))

import Statebox.Core.Transaction (HashStr, InitialTx, TxId, TxSum(..), isUberRootHash)
import Statebox.TransactionStore.Types (Actions, put)

data ProcessError
  = NoUberRoot
  | InitialPreviousNoUberRoot TxId

processTxSum :: HashStr -> TxSum -> Either ProcessError (Actions Unit)
processTxSum hash = case _ of
  UberRootTxInj           -> Left NoUberRoot
  InitialTxInj  initialTx -> processInitialTx hash initialTx
  WiringTxInj   wiringTx  -> ?w
  FiringTxInj   firingTx  -> ?f

processInitialTx :: HashStr -> InitialTx -> Either ProcessError (Actions Unit)
processInitialTx hash initialTx =
  if (isUberRootHash initialTx.previous)
  then Right $ put hash $ InitialTxInj initialTx
  else Left $ InitialPreviousNoUberRoot initialTx.previous
