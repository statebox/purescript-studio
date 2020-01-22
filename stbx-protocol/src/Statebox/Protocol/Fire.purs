module Statebox.Protocol.Fire where

import Statebox.Core.Transaction (FiringTx)

fire :: ∀ a . FiringTx -> a
fire = ?fire
