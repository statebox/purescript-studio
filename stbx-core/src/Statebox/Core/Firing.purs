module Statebox.Core.Firing
  ( Firing
  ) where

import Data.Maybe (Maybe)

import Statebox.Core.Wiring (GluedTransitionIdRaw)
import Statebox.Core.Common (Message, Singleton, TxId)

type Firing =
  { message   :: Maybe Message
  , execution :: Maybe TxId                     -- ^ The execution that this firing is a descendent of.
  , path      :: Singleton GluedTransitionIdRaw -- ^ The net and transition to fire.
  }
