module Statebox.Core.Firing
  ( Firing
  ) where

import Prelude (class Eq, class Show, show, (<>))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Statebox.Core.Wiring (GluedTransitionIdRaw)
import Statebox.Core.Common (Message, Singleton, TxId)

type Firing =
  { message   :: Maybe Message
  , execution :: Maybe TxId                     -- ^ The execution that this firing is a descendent of.
  , path      :: Singleton GluedTransitionIdRaw -- ^ The net and transition to fire.
  }
