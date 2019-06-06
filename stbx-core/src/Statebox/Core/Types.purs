module Statebox.Core.Types where

import Prelude
import Data.Newtype (class Newtype, unwrap)

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
type Wiring =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array Int
  }

type Net =
  { name       :: String
  , partition  :: Array Int
  , names      :: Array String          -- ^ transition names
--, placeNames :: Maybe (Array String)  -- ^ TODO issue #140
  }

type Diagram =
  { name   :: String
  , width  :: Int
  , pixels :: Array Int
  , names  :: Array String
  }

--------------------------------------------------------------------------------

type Firing =
  { message :: Message
  , path    :: Singleton GluedTransitionIdRaw
  }

-- TODO This should be the newtype GluedTransitionId.
type GluedTransitionIdRaw = Int

type HexStr = String

-- | This tags an Array that is expected (but not guaranteed) to have exactly one element. (TODO: newtype.)
type Singleton = Array
