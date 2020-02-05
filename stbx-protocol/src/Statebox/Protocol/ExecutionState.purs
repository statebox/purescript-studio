module Statebox.Protocol.ExecutionState where

import Statebox.Core.Transaction (TxId)
import Statebox.Core.Types (PID)
import Data.Petrinet.Representation.Marking (MarkingF)

type ExecutionState =
  { lastFiring :: TxId
  , wiring     :: TxId
  , marking    :: Marking
  }

-- TODO dedupe
type Marking = MarkingF PID Tokens
type Tokens = Int
