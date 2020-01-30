module Statebox.Core.Types
  ( module Statebox.Core.Wiring
  , module Statebox.Core.Net
  , module Statebox.Core.Diagram
  , Firing
  , GluedTransitionIdRaw
  , GluedTransitionId(..)
  , HexStr
  , Initial(..)
  , Message
  , Singleton
  , TxId
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)

import Statebox.Core.Net (Net, PID, TID, unTID)
import Statebox.Core.Diagram (Diagram)
import Statebox.Core.Wiring (Wiring)

--------------------------------------------------------------------------------

type Initial =
  { message :: Message
  }

-- | Some transaction types have a 'message' field, and they have this type.
type Message = String

--------------------------------------------------------------------------------

type Firing =
  { message   :: Maybe Message
  , execution :: Maybe TxId                     -- ^ The execution that this firing is a descendent of.
  , path      :: Singleton GluedTransitionIdRaw -- ^ The net and transition to fire.
  }

type TxId = String

type HexStr = String

-- | This tags an Array that is expected (but not guaranteed) to have exactly one element. (TODO: newtype.)
type Singleton = NonEmpty Array

-- TODO This should be the newtype GluedTransitionId.
type GluedTransitionIdRaw = Int

--------------------------------------------------------------------------------

newtype GluedTransitionId = GluedTransitionId TID

derive instance newtypeGluedTransitionId :: Newtype GluedTransitionId _

derive instance eqGluedTransitionId :: Eq GluedTransitionId

instance showGluedTransitionId :: Show GluedTransitionId where
  show (GluedTransitionId i) = "(GluedTransitionId " <> show i <> ")"
