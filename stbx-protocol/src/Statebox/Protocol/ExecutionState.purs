module Statebox.Protocol.ExecutionState where

import Statebox.Core.Transaction (TxId)
import View.Petrinet.Model (Marking)

type ExecutionState =
  { lastFiring :: TxId
  , wiring     :: TxId
  , marking    :: Marking
  }
