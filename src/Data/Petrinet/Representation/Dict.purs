module Data.Petrinet.Representation.Dict
  ( NetObjF
  , NetRepF
  , NetApiF
  , mkNetObjF

  , MarkingF
  , mkMarkingF
  , unMarkingF

  , TransitionF
  , PlaceMarkingF

  , fire
  , fireAtMarking
  , findTokens
  , findTokens'
  , isTransitionEnabled

  , preMarking
  , postMarking
  , trMarking
  ) where

import Prelude hiding ((-))
import Data.Foldable (all)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec2D)
import Data.Ring hiding ((-)) -- take (-) from Group.inverse instead TODO why is Group not in Prelude? https://pursuit.purescript.org/packages/purescript-group
import Data.Group (class Group, ginverse)
import Data.Bag (BagF(..))
import Data.Bag as Bag

type MarkingF a n = BagF a n

mkMarkingF :: ∀ a b. Map a b -> BagF a b
mkMarkingF = BagF

unMarkingF :: ∀ a b. BagF a b -> Map a b
unMarkingF (BagF dict) = dict

--------------------------------------------------------------------------------

-- | A representation of a petri net.
type NetRepF pid tid tok r =
  { places                :: Array pid
  , transitionsDict       :: Map tid (TransitionF pid tok)
  , placeLabelsDict       :: Map pid String

  , marking               :: MarkingF pid tok

  , placePointsDict       :: Map pid Vec2D
  , transitionPointsDict  :: Map tid Vec2D
  | r
  }

-- | A NetRepF with some associated API operations.
type NetObjF pid tid tok = NetRepF pid tid tok
  ( findTransition        :: tid -> Maybe (TransitionF pid tok)
  , findPlaceLabel        :: pid -> Maybe String

  , findPlacePoint        :: pid -> Maybe Vec2D
  , findTransitionPoint   :: tid -> Maybe Vec2D
  )

-- TODO was the idea to converge on this one in favour of NetObjF?
type NetApiF pid tid tok =
  { findTokens :: pid -> tok
  }

--------------------------------------------------------------------------------

mkNetObjF :: forall pid tid tok. Ord pid => Ord tid => NetRepF pid tid tok () -> NetObjF pid tid tok
mkNetObjF x =
  { places               : x.places
  , transitionsDict      : x.transitionsDict
  , marking              : x.marking
  , transitionPointsDict : x.transitionPointsDict
  , placeLabelsDict      : x.placeLabelsDict
  , placePointsDict      : x.placePointsDict

  -- API, sort of
  , findTransition       : flip Map.lookup x.transitionsDict
  , findPlaceLabel       : flip Map.lookup x.placeLabelsDict

  -- rendering related
  , findPlacePoint       : flip Map.lookup x.placePointsDict
  , findTransitionPoint  : flip Map.lookup x.transitionPointsDict
  }

--------------------------------------------------------------------------------

type TransitionF p tok =
  { pre  :: Array (PlaceMarkingF p tok)
  , post :: Array (PlaceMarkingF p tok)
  }

--------------------------------------------------------------------------------

type PlaceMarkingF p tok =
  { place  :: p
  , tokens :: tok
  }

fromPlaceMarking :: ∀ a b. PlaceMarkingF a b -> a /\ b
fromPlaceMarking pm = pm.place /\ pm.tokens

preMarking :: ∀ p tok. Ord p => Group (MarkingF p tok) => TransitionF p tok -> MarkingF p tok
preMarking tr = ginverse (trMarking tr.pre)

postMarking :: ∀ p tok. Ord p => TransitionF p tok -> MarkingF p tok
postMarking tr = trMarking tr.post

trMarking :: ∀ p tok. Ord p => Array (PlaceMarkingF p tok) -> MarkingF p tok
trMarking pms = mkMarkingF $ Map.fromFoldable $ fromPlaceMarking <$> pms

--------------------------------------------------------------------------------

fire
  :: ∀ p t tok
   . Ord p
  => Semiring tok
  => Group (MarkingF p tok)
  => NetObjF p t tok
  -> TransitionF p tok
  -> NetObjF p t tok
fire net t = net { marking = fireAtMarking net.marking t }

fireAtMarking
  :: ∀ p tok
   . Ord p
  => Semiring tok
  => Group (MarkingF p tok)
  => MarkingF p tok
  -> TransitionF p tok
  -> MarkingF p tok
fireAtMarking marking t =
  preMarking t <> marking <> postMarking t

--------------------------------------------------------------------------------

findTokens
  :: ∀ p t tok
   . Ord p
  => Monoid (Additive tok)
  => NetObjF p t tok
  -> p
  -> tok
findTokens net = findTokens' net.marking

findTokens'
  :: ∀ p tok
   . Ord p
  => Monoid (Additive tok)
  => MarkingF p tok
  -> p
  -> tok
findTokens' marking place = unwrap $ fromMaybe mempty $ map Additive $ Map.lookup place (unMarkingF marking)

--------------------------------------------------------------------------------

-- TODO use `findTokens' (`mark`?) or Marking/Bag-related functions
isTransitionEnabled :: ∀ pid tok. Ord tok => Ord pid => MarkingF pid tok -> TransitionF pid tok -> Boolean
isTransitionEnabled marking t = isPlaceEnabled `all` t.pre
  where
    isPlaceEnabled :: PlaceMarkingF pid tok -> Boolean
    isPlaceEnabled tp = fromMaybe false $ (>=) <$> mark tp.place <*> Just tp.tokens

    mark :: pid -> Maybe tok
    mark = Bag.lookup' marking
