module Statebox.Core.Firing
  ( Firing
  , GluedTransitionIdRaw
  , GluedTransitionId(..)
  ) where

import Prelude (class Eq, class Show, show, (<>))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Statebox.Core.Net (TID)
import Statebox.Core.Common (Message, Singleton, TxId)

type Firing =
  { message   :: Maybe Message
  , execution :: Maybe TxId                     -- ^ The execution that this firing is a descendent of.
  , path      :: Singleton GluedTransitionIdRaw -- ^ The net and transition to fire.
  }

-- TODO This should be the newtype GluedTransitionId.
type GluedTransitionIdRaw = Int

--------------------------------------------------------------------------------

newtype GluedTransitionId = GluedTransitionId TID

derive instance newtypeGluedTransitionId :: Newtype GluedTransitionId _

derive instance eqGluedTransitionId :: Eq GluedTransitionId

instance showGluedTransitionId :: Show GluedTransitionId where
  show (GluedTransitionId i) = "(GluedTransitionId " <> show i <> ")"
