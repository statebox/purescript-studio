module Data.Petrinet.Representation.PNPROtoDict where

import Prelude
import Data.Array ((..), catMaybes, filter, length, zip, partition)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typedef (Typedef(..))
import Data.Vec3 (Vec2D, Box(..), vec2)

import View.Petrinet.Model as Model
import View.Petrinet.Model (PID, NetRep, NetInfo, PlaceMarking, Tokens, mkNetRep, mkNetInfo)
import Data.Petrinet.Representation.Marking as Marking
import Data.Petrinet.Representation.Marking (MarkingF)
import Data.Petrinet.Representation.PNPRO

toNetInfo :: GSPN -> NetInfo
toNetInfo gspn = mkNetInfo (toNetRep gspn) gspn.name (toModelTextBox <$> gspn.nodes.textBox)

toNetRep :: GSPN -> NetRep
toNetRep gspn =
  mkNetRep
    pids -- TODO not really used in mkNetRep
    transitions'
    marking
    placeLabels
    (pure placePoints)
    transitionLabels
    (Typedef "1" <$ transitions)
    (pure $ toVec2D <$> transitions)
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

    marking :: MarkingF PID Tokens
    marking = Marking.fromFoldable $ (map _.marking) <$> filter (\(_ /\ p) -> p.marking > 0) placesIndexed

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
            preArcMaybe arc = if not arc.isPost then (\pid -> arc.head /\ { place: pid, tokens: arc.mult }) <$> Map.lookup arc.tail pidIndex
                                                else Nothing

    postArcsDict :: Map PidOrTid  (Array PlaceMarking)
    postArcsDict = Map.fromFoldableWith append $ map (\(k /\ v) -> k /\ [v]) postTransitions
      where
        postTransitions :: Array (PidOrTid /\ PlaceMarking)
        postTransitions = catMaybes $ postArcMaybe <$> gspn.edges.arc
          where
            postArcMaybe :: _ -> Maybe (PidOrTid /\ PlaceMarking)
            postArcMaybe arc = if arc.isPost then (\p -> arc.tail /\ { place: p, tokens: arc.mult }) <$> Map.lookup arc.head pidIndex
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
toVec2D v = vec2 v.x v.y

toModelTextBox :: TextBox -> Model.TextBox
toModelTextBox v =
  { name: v.name
  , text: v.text
  , box:  Box { topLeft:     vec2  v.x             v.y
              , bottomRight: vec2 (v.x + v.width) (v.y + v.height)
              }
  }
