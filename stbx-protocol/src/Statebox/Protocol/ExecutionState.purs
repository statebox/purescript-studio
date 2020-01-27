module Statebox.Protocol.ExecutionState where

import Statebox.Core.Transaction (TxId)

type ExecutionState =
  { lastFiring :: TxId
  , wiring     :: TxId
  }
