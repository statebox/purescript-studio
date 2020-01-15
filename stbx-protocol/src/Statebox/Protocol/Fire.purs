module Statebox.Protocol.Fire where

import Statebox.Core.Transaction (FiringTx)

fire :: forall a . FiringTx -> a
fire = ?fire