module Model where

import Prelude
import Data.Array ((..), length)
import Data.Bag (BagF(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))

import Data.Petrinet.Representation.Dict (TransitionF, MarkingF, PlaceMarkingF, findTokens', NetRepF, NetObjF, NetApiF, mkNetObjF)
import Data.Vec2D (Vec2D)

data QueryF pid tid a
  = LoadNet (NetObjF pid tid Tokens Typedef) a
  | FireTransition tid a
  | FocusTransition tid a
  | FocusPlace pid a
  | UpdatePlace (PlaceQueryF pid a)
  | UpdateTransition (TransitionQueryF tid a)

data PlaceQueryF pid a
  = UpdatePlaceLabel String a

data TransitionQueryF tid a
  = UpdateTransitionName String a
  | UpdateTransitionType Typedef a

newtype Typedef = Typedef String

derive instance newtypeTypedef :: Newtype (Typedef)  _

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

type NetInfoFRow pid tid r =
  ( name   :: String
  , net    :: NetObjF pid tid Tokens Typedef
  , netApi :: NetApiF pid tid Tokens
  | r
  )

type NetInfoF pid tid r = Record (NetInfoFRow pid tid r)

-- types specialised to Int index ----------------------------------------------

type PID          = Int
type TID          = Int
type Tokens       = Int

type Transition   = TransitionF   PID Tokens
type Marking      = MarkingF      PID Tokens
type PlaceMarking = PlaceMarkingF PID Tokens

type NetRep = NetRepF PID TID Tokens Typedef ()

type NetObj = NetObjF PID TID Tokens Typedef

type NetApi = NetApiF PID TID Tokens

type NetInfo = Record (NetInfoFRow PID TID ())

-- empty net -------------------------------------------------------------------

emptyNetData :: NetRep
emptyNetData = mkNetRep mempty mempty (BagF mempty) mempty mempty mempty mempty mempty

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
  -> Array String
  -> Array Typedef
  -> Array Vec2D
  -> NetRep
mkNetRep places transitions marking placeLabels placePoints transitionLabels transitionTypes transitionPoints =
  { places:               places
  , transitionsDict:      transitionsDict
  , marking:              marking
  , placeLabelsDict:      placeLabelsDict
  , placePointsDict:      placePointsDict
  , transitionLabelsDict: transitionLabelsDict
  , transitionTypesDict:  transitionTypesDict
  , transitionPointsDict: transitionPointsDict
  }
  where
    -- TODO check +1
    firstTransitionIndex = length places + 1

    transitionsDict :: Map Int Transition
    transitionsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitions

    placeLabelsDict :: Map Int String
    placeLabelsDict = Map.fromFoldable placeLabels

    placePointsDict = Map.fromFoldable placePoints

    transitionLabelsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionLabels

    transitionTypesDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionTypes

    transitionPointsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionPoints

--------------------------------------------------------------------------------

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Int /\ v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> (i0+i) /\ x) xs
