module View.Petrinet.Model where

import Prelude
import Data.Array ((..), length)
import Data.Bag (BagF(..))
import Data.Foldable (foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, ala)
import Data.Ord.Max (Max(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))

import Data.Auth (Role, Roles, RoleInfo)
import Data.Typedef (Typedef(..), TypeName)
import Data.Petrinet.Representation.Dict as Dict
import Data.Petrinet.Representation.Dict (TransitionF, PlaceMarkingF, NetRepF, NetApiF, mkNetApiF)
import Data.Petrinet.Representation.Layout as Layout
import Data.Petrinet.Representation.Layout (NetLayoutF)
import Data.Petrinet.Representation.Marking as Marking
import Data.Petrinet.Representation.Marking (MarkingF)
import Data.Vec3 as Vec3
import Data.Vec3 (Vec2D, Box, Vec3, minMax)
import Data.Vec3 (bounds, minMaxZero) as Vec2D
import Data.Vec3.Box as Box

import Statebox.Core.Types (PID, TID, Tokens) as Core

data Action pid tid ty2
  = LoadNet (NetInfoWithTypesAndRolesF pid tid Typedef ty2 ())
  | FireTransition tid
  | FocusTransition tid
  | FocusPlace pid
  | UpdatePlace (PlaceAction pid)
  | UpdateTransition (TransitionAction tid)
  | ToggleLabelVisibility NetElemKind

data PlaceAction pid
  = UpdatePlaceLabel pid String

data TransitionAction tid
  = UpdateTransitionName tid String
  | UpdateTransitionType tid Typedef

--------------------------------------------------------------------------------

data NetElemKind = Arc | Place | Transition

type TextBoxF n =
  { name   :: String
  , text   :: String
  , box    :: Box n
  }

type TextBox = TextBoxF Number

-- TODO lensify and decompose
mapTextBoxF :: ∀ a b. (Vec3 a -> Vec3 b) -> TextBoxF a -> TextBoxF b
mapTextBoxF f b = b { box = Box.mapVec3s f b.box }

--------------------------------------------------------------------------------

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

--------------------------------------------------------------------------------

type NetInfoFRow pid tid ty r =
  ( name      :: String
  , net       :: NetRepF pid tid Tokens ty ()
  , netApi    :: NetApiF pid tid Tokens
  , textBoxes :: Array TextBox
  | r
  )

type NetInfoF pid tid ty r = Record (NetInfoFRow pid tid ty r)

-- | This extends a net with fields containing project-wide info.
type NetInfoWithTypesAndRolesFRow pid tid ty ty2 r =
  NetInfoFRow pid tid ty
    ( types     :: Array (TypeName /\ ty2)
    , roleInfos :: Array RoleInfo
    | r
    )

type NetInfoWithTypesAndRolesF pid tid ty ty2 r = Record (NetInfoWithTypesAndRolesFRow pid tid ty ty2 r)

-- types specialised to Int index ----------------------------------------------

type PID          = Core.PID    -- TODO remove this redirect; clients should import this directly
type TID          = Core.TID    -- TODO remove this redirect; clients should import this directly
type Tokens       = Core.Tokens -- TODO remove this redirect; clients should import this directly

type Transition   = TransitionF   PID Tokens
type Marking      = MarkingF      PID Tokens
type PlaceMarking = PlaceMarkingF PID Tokens

type NetRep = NetRepF PID TID Tokens Typedef ()

type NetApi = NetApiF PID TID Tokens

type NetInfo = Record (NetInfoFRow PID TID Typedef ())

type NetLayout = NetLayoutF PID TID

--------------------------------------------------------------------------------

mkNetRep
  :: Array PID
  -> Array Transition
  -> Marking
  -> Array (PID /\ String)
  -> Maybe (Array (PID /\ Vec2D))
  -> Array String
  -> Array Typedef
  -> Maybe (Array Vec2D)
  -> Array Roles
  -> NetRep
mkNetRep pids transitions marking placeLabels placePoints transitionLabels transitionTypes transitionPoints transitionAuths =
  mkNetRepUsingLayout pids transitions marking placeLabels transitionLabels layout transitionTypes transitionAuths
  where
    layout = mkLayout <$> pure firstTransitionIndex <*> placePoints <*> transitionPoints
    firstTransitionIndex = length pids + 1

mkNetRepUsingLayout
  :: Array PID
  -> Array Transition
  -> Marking
  -> Array (PID /\ String)
  -> Array String
  -> Maybe NetLayout
  -> Array Typedef
  -> Array Roles
  -> NetRep
mkNetRepUsingLayout pids transitions marking placeLabels transitionLabels layout transitionTypes transitionAuths =
  { places:               pids
  , placeLabelsDict:      placeLabelsDict

  , transitionsDict:      labelTransitionsWith transitions
  , transitionLabelsDict: labelTransitionsWith transitionLabels
  , transitionTypesDict:  labelTransitionsWith transitionTypes
  , transitionAuthsDict:  labelTransitionsWith transitionAuths

  , layout:               layout

  , marking:              marking
  }
  where
    firstTransitionIndex = length pids + 1

    placeLabelsDict :: Map Int String
    placeLabelsDict = Map.fromFoldable placeLabels

    labelTransitionsWith :: forall a. Array a -> Map TID a
    labelTransitionsWith = Map.fromFoldable <<< zipWithIndexFrom firstTransitionIndex

mkLayout
  :: Int
  -> Array (PID /\ Vec2D)
  -> Array Vec2D
  -> NetLayout
mkLayout firstTransitionIndex placePoints transitionPoints =
   { placePointsDict:      Map.fromFoldable placePoints
   , transitionPointsDict: Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionPoints
   , edgeWaypointsDict:    Map.empty
   }

mkNetApi :: NetRep -> NetApi
mkNetApi = mkNetApiF

mkNetInfo :: NetRep -> String -> Array TextBox -> NetInfo
mkNetInfo net name textBoxes =
  { name:      name
  , net:       net
  , textBoxes: textBoxes
  , netApi:    mkNetApi net
  }

--------------------------------------------------------------------------------

type Visuals pid tid =
  { layout    :: NetLayoutF pid tid
  , textBoxes :: Array TextBox
  }

mapPoints :: ∀ pid tid. (Vec2D -> Vec2D) -> Visuals pid tid -> Visuals pid tid
mapPoints f { layout, textBoxes } =
  { layout:    Layout.mapVec2D f layout
  , textBoxes: mapTextBoxF f <$> textBoxes
  }

translateAndScale :: ∀ pid tid. Number -> Visuals pid tid -> Visuals pid tid
translateAndScale factor n =
  mapPoints (\v -> (v + vTranslate) * vScale) n
  where
    vScale :: Vec2D
    vScale = pure (factor / maxBound)

    -- translate to/center around origin
    vTranslate :: Vec2D
    vTranslate = (-bounds.min) - (boundsDelta / pure 2.0)

    maxBound :: Number
    maxBound = ala Max foldMap $ Vec3.toArray boundsDelta

    boundsDelta :: Vec2D
    boundsDelta = bounds.max - bounds.min

    bounds :: { min :: Vec2D, max :: Vec2D }
    bounds = boundingBox n

boundingBox :: ∀ pid tid. Visuals pid tid -> { min :: Vec2D, max :: Vec2D }
boundingBox { layout, textBoxes } =
  layoutBounds `minMax` textBoxesBounds
  where
    layoutBounds = Layout.bounds layout
    textBoxesBounds = Vec2D.bounds ((Box.toArray <<< _.box) =<< textBoxes)

--------------------------------------------------------------------------------

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Int /\ v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> (i0+i) /\ x) xs
