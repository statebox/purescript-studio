module Data.Petrinet.Representation.PNPRO where

import Prelude

import Data.Array ((..), catMaybes, filter, length, zip, partition)
import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))

import Data.Petrinet.Representation.Marking as Marking
import Data.Petrinet.Representation.Marking (MarkingF)
import Data.Vec3 (Vec2D, Box(..), vec2)
import View.Petrinet.Model (PID, Typedef(..), NetRep, NetInfo, PlaceMarking, Tokens, mkNetRep, mkNetInfo)
import View.Petrinet.Model as Model

--------------------------------------------------------------------------------

foreign import fromString :: String -> Effect Document

foreign import fromStringUnsafe :: String -> Document

--------------------------------------------------------------------------------

type Document = { project :: Project }

type Project =
  { name    :: String
  , version :: String -- ^ TODO should be Int
  , gspn    :: Array GSPN
  }

type GSPN =
  { name  :: String
  , nodes :: Nodes
  , edges :: { arc :: Array Arc }
  }

type Nodes =
  { place      :: Array Place
  , transition :: Array Transition
  , textBox    :: Array TextBox
  }

type Node r =
  { name    :: String
  , x       :: Number
  , y       :: Number
  | r
  }

type Place = Node
  ( marking :: Tokens -- TODO it looks like this appears in the js data as ("1" :: String), but also as (0 :: Int)
  )

type Transition = Node
  ( "type" :: String -- ^ TODO should be an ADT: "EXP", ...?
  )

type TextBox = Node
  ( text   :: String
  , height :: Number
  , width  :: Number
  )

type PidOrTid = String

type Arc =
  { head   :: PidOrTid
  , tail   :: PidOrTid
  , kind   :: String   -- ^ TODO should be an ADT: "INPUT", "OUTPUT", ...?
  , isPost :: Boolean
  , mult   :: Int      -- ^ Multiplicity
  }
