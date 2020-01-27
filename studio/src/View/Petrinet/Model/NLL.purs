-- | Convert NLL-encoded Petri nets to view models (`NetInfo`, `NetRep`, etc.).
module View.Petrinet.Model.NLL where

import Prelude

import Data.Auth (Roles)
import Data.Array ((..), length, zip)
import Data.ArrayMultiset as ArrayMultiset
import Data.ArrayMultiset (ArrayMultiset)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

import Data.Petrinet.Representation.NLL as NLL
import Data.Petrinet.Representation.NLL (NetF)
import Data.Typedef (Typedef(..))
import View.Petrinet.Model (PID, Transition, NetRep, NetInfo, mkNetRep, mkNetInfo)

toNetInfo :: NetF PID -> String -> Array String -> Array String -> Array Typedef -> Array Roles -> NetInfo
toNetInfo net name placeNames transitionNames typedefs roles =
  mkNetInfo netRep name mempty
  where
    netRep = toNetRep net placeNames transitionNames typedefs roles

toNetInfoWithDefaults :: NetF PID -> String -> Array String -> Array String -> NetInfo
toNetInfoWithDefaults net name placeNames transitionNames =
  mkNetInfo netRep name mempty
  where
    netRep = toNetRepWithDefaults net placeNames transitionNames

toNetRepWithDefaults :: NetF PID -> Array String -> Array String -> NetRep
toNetRepWithDefaults net placeNames transitionNames =
  toNetRep net placeNames transitionNames typedefs roles
  where
    transitions = net
    typedefs    = Typedef "1" <$ transitions
    roles       = mempty

toNetRep :: NetF PID -> Array String -> Array String -> Array Typedef -> Array Roles -> NetRep
toNetRep net placeNames transitionNames typedefs roles =
  mkNetRep places
           transitions
           mempty
           placeNamesIndexed
           Nothing
           transitionNames
           typedefs
           Nothing
           roles
  where
    places            :: Array PID
    places            = Set.toUnfoldable $ NLL.uniquePlaceIds net

    firstPlaceIndex   = 1
    numPlaces         = length places
    pids              = firstPlaceIndex .. numPlaces

    transitions       = mkTransition <$> net

    placeNamesIndexed = pids `zip` placeNames

    mkTransition :: ArrayMultiset PID /\ ArrayMultiset PID -> Transition
    mkTransition (pre /\ post) =
      { pre:  (\(place /\ tokens) -> { place, tokens }) <$> ArrayMultiset.count pre
      , post: (\(place /\ tokens) -> { place, tokens }) <$> ArrayMultiset.count post
      }

--------------------------------------------------------------------------------

defaultPlaceNames :: ∀ a. Ord a => Eq a => NetF a -> Array String
defaultPlaceNames net = defaultPlaceNames' numPlaces
  where
    numPlaces = length $ Set.toUnfoldable $ NLL.uniquePlaceIds net

defaultPlaceNames' :: Int -> Array String
defaultPlaceNames' numPlaces = (append "p" <<< show) <$> (0 .. (numPlaces-1))
