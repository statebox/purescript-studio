module Data.Petrinet.Representation.PNPRO where

import Prelude

import Control.MonadZero (guard)
import Data.Array ((..), catMaybes, filter, length, zip, partition)
import Data.Bag as Bag
import Data.Bag (BagF)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Traversable (traverse)

import Data.Map as Map
import Data.Map (Map)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec2D)

import Data.Auth as Auth
import Data.Petrinet.Representation.Dict (mkNetObjF)
import View.Petrinet.Model (PID, TID, Typedef(..), NetRep, NetApi, NetInfo, PlaceMarking, Tokens, mkNetRep, mkNetApi, emptyNetApi)
import View.Petrinet.Model as Model

--------------------------------------------------------------------------------

-- | TODO Token weight should be configurable; find out how token weights > 1
-- | are stored in PNPRO XML (I would expect as a 'marking' attr on an <arc/>).
hardcodedArcWeightTodo :: Tokens
hardcodedArcWeightTodo = 1

--------------------------------------------------------------------------------

-- | TODO return some effect type, reify exceptions, etc.
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
  ( height :: Number
  , width  :: Number
  )

type PidOrTid = String

type Arc =
  { head   :: PidOrTid
  , tail   :: PidOrTid
  , kind   :: String -- ^ TODO should be an ADT: "INPUT", "OUTPUT", ...?
  , isPost :: Boolean
  }

--------------------------------------------------------------------------------

mkNetInfo :: GSPN -> NetInfo
mkNetInfo gspn = { name: gspn.name, net: mkNetObjF netRep, netApi: mkNetApi netRep }
  where
    netRep = toNetRep gspn

toNetRep :: GSPN -> NetRep
toNetRep gspn =
  mkNetRep
    pids -- TODO not really used in mkNetRep
    transitions'
    marking
    placeLabels
    placePoints
    transitionLabels
    (const (Typedef "Unit") <$> transitions)
    (toVec2D <$> transitions)
    transitionAuthsDict
  where
    firstPlaceIndex      = 1
    numPlaces            = length places
    pids                 = firstPlaceIndex .. numPlaces
    places               = gspn.nodes.place
    firstTransitionIndex = firstPlaceIndex + numPlaces
    numTransitions       = length transitions
    tids                 = firstTransitionIndex .. (firstTransitionIndex + numTransitions -1)
    transitions          = gspn.nodes.transition

    placesIndexed :: Array (PID /\ Place)
    placesIndexed = zipWithIndexFrom firstPlaceIndex places

    marking :: BagF PID Tokens
    marking = Bag.fromFoldable $ (map _.marking) <$> filter (\(_ /\ p) -> p.marking > 0) placesIndexed

    placeLabels :: Array (PID /\ PidOrTid)
    placeLabels = map _.name <$> placesIndexed

    placePoints :: Array (PID /\ Vec2D)
    placePoints = map toVec2D <$> placesIndexed

    -- TODO StrMap?
    pidIndex :: Map PidOrTid Int
    pidIndex = Map.fromFoldable $ zip (_.name <$> places) pids

    { yes: postArcs, no: preArcs } = partition _.isPost gspn.edges.arc

    preArcsDict :: Map PidOrTid  (Array PlaceMarking)
    preArcsDict  = Map.fromFoldableWith append $ map (\(k /\ v) -> k /\ [v]) preTransitions
      where
        preTransitions :: Array (PidOrTid /\ PlaceMarking)
        preTransitions = catMaybes $ preArcMaybe <$> gspn.edges.arc
          where
            -- TODO token weight should be configurable; find out how token weights > 1 are stored in PNPRO XML (I would expect as a 'marking' attr on an <arc/>)
            preArcMaybe :: _ -> Maybe (PidOrTid /\ PlaceMarking)
            preArcMaybe arc = if not arc.isPost then (\pid -> arc.head /\ { place: pid, tokens: hardcodedArcWeightTodo }) <$> Map.lookup arc.tail pidIndex
                                                else Nothing

    postArcsDict :: Map PidOrTid  (Array PlaceMarking)
    postArcsDict = Map.fromFoldableWith append $ map (\(k /\ v) -> k /\ [v]) postTransitions
      where
        postTransitions :: Array (PidOrTid /\ PlaceMarking)
        postTransitions = catMaybes $ postArcMaybe <$> gspn.edges.arc
          where
            postArcMaybe :: _ -> Maybe (PidOrTid /\ PlaceMarking)
            postArcMaybe arc = if arc.isPost then (\p -> arc.tail /\ { place: p, tokens: hardcodedArcWeightTodo }) <$> Map.lookup arc.head pidIndex
                                             else Nothing

    transitionLabels = (_.name <$> transitions)

    transitions' :: Array Model.Transition
    transitions' = mkTransition' <$> transitions

    mkTransition' :: Transition -> Model.Transition
    mkTransition' tr =
      { pre:  fromMaybe [] <<< flip Map.lookup preArcsDict  $ tr.name
      , post: fromMaybe [] <<< flip Map.lookup postArcsDict $ tr.name
      }

    transitionAuthsDict = mempty

--------------------------------------------------------------------------------

-- TODO dedupe; also in Model.purs
zipWithIndexFrom :: forall v. Int -> Array v -> Array (Int /\ v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> (i0+i) /\ x) xs

-- TODO It'd be faster to make the Vec2D row type extensible, so it can include other
-- fields. Will that work if Vec2D is/becomes a newtype though?
toVec2D :: forall r. { x :: Number, y :: Number | r } -> Vec2D
toVec2D v = { x: v.x, y: v.y }
