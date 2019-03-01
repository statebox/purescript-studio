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
import Data.Typedef.Typedef2 (Typedef2, TypeName)
import Data.Vec3 (Vec2, Vec2D, Box(..))

data QueryF pid tid a
  = LoadNet (NetInfoWithTypesAndRolesF pid tid Typedef Typedef2 ()) a
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

--------------------------------------------------------------------------------

newtype Typedef = Typedef String

derive instance newtypeTypedef :: Newtype Typedef _

--------------------------------------------------------------------------------

type TextBoxF n =
  { name   :: String
  , box    :: Box n
  }

type TextBox = TextBoxF Number

--------------------------------------------------------------------------------

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

--------------------------------------------------------------------------------

type NetInfoFRow pid tid ty r =
  ( name      :: String
  , net       :: NetObjF pid tid Tokens ty
  , netApi    :: NetApiF pid tid Tokens
  , textBoxes :: Array TextBox
  | r
  )

type NetInfoF pid tid ty r = Record (NetInfoFRow pid tid ty r)

-- | This extends a net with fields containing project-wide info.
type NetInfoWithTypesAndRolesFRow pid tid ty ty2 r = NetInfoFRow pid tid ty
  ( types     :: Array (TypeName /\ ty2)
  , roleInfos :: Array RoleInfo
  | r
  )

type NetInfoWithTypesAndRolesF pid tid ty ty2 r = Record (NetInfoWithTypesAndRolesFRow pid tid ty ty2 r)

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

type NetInfo = Record (NetInfoFRow PID TID Typedef ())

type NetInfoWithTypesAndRoles = Record (NetInfoWithTypesAndRolesFRow PID TID Typedef Typedef2 ())

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
  -> Array Roles
  -> NetRep
mkNetRep pids transitions marking placeLabels placePoints transitionLabels transitionTypes transitionPoints transitionAuths =
  { places:               pids
  , transitionsDict:      transitionsDict
  , marking:              marking
  , placeLabelsDict:      placeLabelsDict
  , placePointsDict:      placePointsDict
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

    transitionLabelsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionLabels

    transitionTypesDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionTypes

    transitionPointsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionPoints

    transitionAuthsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionAuths

mkNetApi :: NetRep -> NetApi
mkNetApi rep =
  { findTokens : findTokens' rep.marking
  }

mkNetInfo :: NetRep -> String -> Array TextBox -> NetInfo
mkNetInfo net name textBoxes =
  { name:      name
  , net:       mkNetObjF net
  , textBoxes: textBoxes
  , netApi:    mkNetApi net
  }

--------------------------------------------------------------------------------

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Int /\ v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> (i0+i) /\ x) xs
