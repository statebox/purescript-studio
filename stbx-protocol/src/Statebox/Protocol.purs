module Statebox.Protocol where

import Data.Unit (Unit)

import Statebox.Core.Transaction (HashStr, TxSum(..))
import Statebox.TransactionStore.Types (Actions)

processTxSum :: HashStr -> TxSum -> Actions Unit
processTxSum hash = case _ of
  UberRootTxInj           -> ?u
  InitialTxInj  initialTx -> ?i
  WiringTxInj   wiringTx  -> ?w
  FiringTxInj   firingTx  -> ?f
