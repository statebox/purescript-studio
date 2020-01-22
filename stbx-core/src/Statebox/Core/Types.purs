module Statebox.Core.Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))

-- | Place id.
type PID = Int

-- TODO newtype
-- | Transition id.
type TID = Int

unTID :: TID -> Int
unTID i = i

-- | Some transaction types have a 'message' field, and they have this type.
type Message = String

--------------------------------------------------------------------------------

newtype GluedTransitionId = GluedTransitionId TID

derive instance newtypeGluedTransitionId :: Newtype GluedTransitionId _

derive instance eqGluedTransitionId :: Eq GluedTransitionId

instance showGluedTransitionId :: Show GluedTransitionId where
  show (GluedTransitionId i) = "(GluedTransitionId " <> show i <> ")"

--------------------------------------------------------------------------------

type Initial =
  { message :: Message
  }

--------------------------------------------------------------------------------

-- | About how wirings are encoded:
-- |
-- | 1) The root (top-level) diagram is always `diagrams[0]`.
-- |
-- | 2) The elements of `labels: [0,0]` are the labels of this root diagram, and they are indices
-- |    into the list `(nets <> diagrams)`.
-- |
-- | for a more detailed description, refer to https://hackmd.io/0CPzJ_V-Qkm0y40NLQRhWw?view#Representing-the-gluing
type Wiring =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array Int
  }

type Net =
  { name       :: String
  , partition  :: Array Int             -- NLL encoding
  , names      :: Array String          -- ^ transition names
  , placeNames :: Maybe (Array String)
  }

type Diagram =
  { name   :: String
  , width  :: Int          -- width of the brick diagram (see https://docs.statebox.org/spec/nlldiagrams/)
  , pixels :: Array Int    -- actual brick diagram encoding
  , names  :: Array String
  }

--------------------------------------------------------------------------------

type Firing =
  { message   :: Maybe Message
  , execution :: Maybe TxId                     -- ^ The execution that this firing is a descendent of.
  , path      :: Singleton GluedTransitionIdRaw -- ^ The net and transition to fire.
  }

-- TODO This should be the newtype GluedTransitionId.
type GluedTransitionIdRaw = Int

type HexStr = String

-- | This tags an Array that is expected (but not guaranteed) to have exactly one element. (TODO: newtype.)
type Singleton = NonEmpty Array

type TxId = String
