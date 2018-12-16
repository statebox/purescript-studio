module View.Petrinet.Model where

import Prelude
import Data.Array ((..), length)
import Data.Bag (BagF(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))

import Data.Auth (Role, Roles, RoleInfo)
import Data.Petrinet.Representation.Dict (TransitionF, MarkingF, PlaceMarkingF, findTokens', NetRepF, NetObjF, NetApiF, mkNetObjF)
import Data.Vec2D (Vec2D, Vec4D)

data QueryF pid tid tbid a
  = LoadNet (NetInfoF pid tid tbid ()) a
  | FireTransition tid a
  | FocusTransition tid a
  | FocusPlace pid a
  | UpdatePlace (PlaceQueryF pid a)
  | UpdateTransition (TransitionQueryF tid a)
  | ToggleLabelVisibility NetElemKind a

data PlaceQueryF pid a
  = UpdatePlaceLabel pid String a

data TransitionQueryF tid a
  = UpdateTransitionName tid String a
  | UpdateTransitionType tid Typedef a

--------------------------------------------------------------------------------

data NetElemKind = Arc | Place | Transition

newtype Typedef = Typedef String

derive instance newtypeTypedef :: Newtype (Typedef)  _

--------------------------------------------------------------------------------

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

--------------------------------------------------------------------------------

type NetInfoFRow pid tid tbid r =
  ( name   :: String
  , net    :: NetObjF pid tid Tokens tbid Typedef
  , netApi :: NetApiF pid tid Tokens
  | r
  )

type NetInfoF pid tid tbid r = Record (NetInfoFRow pid tid tbid r)

-- types specialised to Int index ----------------------------------------------

type PID          = Int
type TID          = Int
type Tokens       = Int
type TBID         = Int

type Transition   = TransitionF   PID Tokens
type Marking      = MarkingF      PID Tokens
type PlaceMarking = PlaceMarkingF PID Tokens

type NetRep = NetRepF PID TID Tokens TBID Typedef ()

type NetObj = NetObjF PID TID Tokens TBID Typedef

type NetApi = NetApiF PID TID Tokens

type NetInfo = Record (NetInfoFRow PID TID TBID ())

-- empty net -------------------------------------------------------------------

emptyNetData :: NetRep
emptyNetData = mkNetRep mempty mempty (BagF mempty) mempty mempty mempty mempty mempty mempty mempty mempty

emptyNet :: NetObj
emptyNet = mkNetObjF emptyNetData

emptyNetApi :: NetApi
emptyNetApi =
  { findTokens : findTokens' emptyNetData.marking
  }

emptyNetInfo :: NetInfo
emptyNetInfo = { net: emptyNet, netApi: emptyNetApi, name: "" }

--------------------------------------------------------------------------------

mkNetRep
  :: Array PID
  -> Array Transition
  -> Marking
  -> Array (PID /\ String)
  -> Array (PID /\ Vec2D)
  -> Array (TBID /\ String)
  -> Array (TBID /\ Vec4D)
  -> Array String
  -> Array Typedef
  -> Array Vec2D
  -> Array Roles
  -> NetRep
mkNetRep pids transitions marking placeLabels placePoints textBoxLabels textBoxes transitionLabels transitionTypes transitionPoints transitionAuths =
  { places:               pids
  , transitionsDict:      transitionsDict
  , marking:              marking
  , placeLabelsDict:      placeLabelsDict
  , placePointsDict:      placePointsDict
  , textBoxLabelsDict:    textBoxLabelsDict
  , textBoxesDict:        textBoxesDict
  , transitionLabelsDict: transitionLabelsDict
  , transitionTypesDict:  transitionTypesDict
  , transitionPointsDict: transitionPointsDict
  , transitionAuthsDict:  transitionAuthsDict
  }
  where
    firstTransitionIndex = length pids + 1

    transitionsDict :: Map Int Transition
    transitionsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitions

    placeLabelsDict :: Map Int String
    placeLabelsDict = Map.fromFoldable placeLabels

    placePointsDict = Map.fromFoldable placePoints

    textBoxLabelsDict :: Map Int String
    textBoxLabelsDict = Map.fromFoldable textBoxLabels

    textBoxesDict = Map.fromFoldable textBoxes

    transitionLabelsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionLabels

    transitionTypesDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionTypes

    transitionPointsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionPoints

    transitionAuthsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionAuths

mkNetApi :: NetRep -> NetApi
mkNetApi rep =
  { findTokens : findTokens' rep.marking
  }

--------------------------------------------------------------------------------

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Int /\ v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> (i0+i) /\ x) xs
