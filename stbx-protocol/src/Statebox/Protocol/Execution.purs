module Statebox.Protocol.Execution where

import Statebox.Core.Transaction (TxId)

type Execution =
  { lastTransition :: TxId
  , wiring         :: TxId
  }
