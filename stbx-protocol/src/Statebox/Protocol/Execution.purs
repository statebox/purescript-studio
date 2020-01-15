module Statebox.Protocol.Execution where

import Statebox.Core.Transaction (TxId)

data Execution = Execution
  { lastTransition :: TxId
  , wiring         :: TxId
  }