module Statebox.Protocol.Execution where

import Statebox.Core.Transaction (TxId)

type Execution =
  { lastFiring :: TxId
  , wiring     :: TxId
  }
