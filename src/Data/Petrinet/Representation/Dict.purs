module Data.Petrinet.Representation.Dict
  ( NetRepF
  , MarkingF

  , mkMarkingF
  , unMarkingF
  , TransitionF
  , PlaceMarkingF

  , fire
  , fireAtMarking
  , findTokens
  , findTokens'

  , preMarking
  , postMarking
  , trMarking
  )
where

import Prelude hiding ((-))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec2D)
import Data.Ring hiding ((-)) -- take (-) from Group.inverse instead TODO why is Group not in Prelude? https://pursuit.purescript.org/packages/purescript-group
import Data.Group
import Data.Bag

type MarkingF a n = BagF a n

mkMarkingF :: ∀ a b. Map a b -> BagF a b
mkMarkingF = BagF

unMarkingF :: ∀ a b. BagF a b -> Map a b
unMarkingF (BagF dict) = dict

--------------------------------------------------------------------------------

type NetRepF pid tid tok =
  { places                :: Array pid
  , transitionsDict       :: Map tid (TransitionF pid tok)
  , labelsDict            :: Map pid String

  , marking               :: MarkingF pid tok

  , placePointsDict       :: Map pid Vec2D
  , transitionPointsDict  :: Map tid Vec2D

  , findTransition        :: tid -> Maybe (TransitionF pid tok)
  , findPlaceLabel        :: pid -> Maybe String

  , findPlacePoint        :: pid -> Maybe Vec2D
  , findTransitionPoint   :: tid -> Maybe Vec2D
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
  => NetRepF p t tok
  -> TransitionF p tok
  -> NetRepF p t tok
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
  => NetRepF p t tok
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
