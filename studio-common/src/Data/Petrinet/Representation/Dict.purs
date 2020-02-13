module Data.Petrinet.Representation.Dict
  ( NetRepF

  , NetApiF
  , mkNetApiF

  , computeTransitionIds
  , computeNumTransitions

  , TransitionF
  , PlaceMarkingF

  , fire
  , fireAtMarking
  , isTransitionEnabled

  , preMarking
  , postMarking
  , trMarking

  , module Data.Petrinet.Representation.Layout -- TODO better to do specific exports (NetLayoutF, etc)
  ) where

import Prelude hiding ((-))
import Data.Array (length)
import Data.Foldable (all)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (Vec2D, Vec2)
import Data.Ring hiding ((-)) -- take (-) from Group.inverse instead TODO why is Group not in Prelude? https://pursuit.purescript.org/packages/purescript-group
import Data.Set as Set
import Data.Group (class Group, ginverse)
import Effect.Aff (Aff, delay, Milliseconds(..))

-- TODO this dependency should probably be eliminated in favour of a type parameter
import Data.Auth as Auth
import Data.Petrinet.Representation.Marking as Marking
import Data.Petrinet.Representation.Marking (MarkingF, tokensAt)
import Data.Petrinet.Representation.Layout

-- | A representation of a Petri net with some additional labelings and metadata.
type NetRepF pid tid tok typ r =
  { places                :: Array pid
  , placeLabelsDict       :: Map pid String

  , transitionsDict       :: Map tid (TransitionF pid tok)
  , transitionLabelsDict  :: Map tid String

  , layout                :: Maybe (NetLayoutF pid tid)

  -- Statebox-specific labelings
  , transitionTypesDict   :: Map tid typ
  , transitionAuthsDict   :: Map tid Auth.Roles

  -- net state (TODO should be segregated from fields related to the net's topology)
  , marking               :: MarkingF pid tok

  | r
  }

--------------------------------------------------------------------------------

-- | An interface through which to interact with a Petri net.
type NetApiF pid tid tok =
  { transition :: tid -> Maybe (TransitionF pid tok)

  -- data that should go through the schema mapping instead
  , placeLabel :: pid -> Maybe String

  -- net state and execution
  , findTokens :: pid -> tok
  , fire :: MarkingF pid tok -> TransitionF pid tok -> Aff (MarkingF pid tok)
  }

mkNetApiF
  :: ∀ pid tid tok typ r
   . Ord pid
  => Ord tid
  => Semiring tok
  => Group (MarkingF pid tok)
  => NetRepF pid tid tok typ r
  -> NetApiF pid tid tok
mkNetApiF rep =
  { transition: \tid -> Map.lookup tid rep.transitionsDict
  , placeLabel: \pid -> Map.lookup pid rep.placeLabelsDict
  , findTokens: Marking.findTokens rep.marking
  , fire: \marking t -> do delay (Milliseconds 500.0)
                           pure $ fireAtMarking marking t
  }

--------------------------------------------------------------------------------

-- TODO Having to compute the transition ids from a labels dictionary seems fairly awkward and slow, so rethink NetRepF
computeTransitionIds :: ∀ pid tid tok typ r. NetRepF pid tid tok typ r -> Array tid
computeTransitionIds net = Set.toUnfoldable $ Map.keys net.transitionLabelsDict

-- TODO see comments on computeTransitionIds
computeNumTransitions :: ∀ pid tid tok typ r. NetRepF pid tid tok typ r -> Int
computeNumTransitions = length <<< computeTransitionIds

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
trMarking pms = Marking.fromFoldable $ fromPlaceMarking <$> pms

--------------------------------------------------------------------------------

fire
  :: ∀ p t tok typ r
   . Ord p
  => Semiring tok
  => Group (MarkingF p tok)
  => NetRepF p t tok typ r
  -> TransitionF p tok
  -> NetRepF p t tok typ r
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

isTransitionEnabled :: ∀ pid tok. Ord tok => Ord pid => MarkingF pid tok -> TransitionF pid tok -> Boolean
isTransitionEnabled marking t = isPlaceEnabled `all` t.pre
  where
    isPlaceEnabled :: PlaceMarkingF pid tok -> Boolean
    isPlaceEnabled tp = fromMaybe false $ (>=) <$> marking `tokensAt` tp.place <*> Just tp.tokens
