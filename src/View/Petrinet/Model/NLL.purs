-- | Convert NLL-encoded Petri nets to view models (`NetInfo`, `NetRep`, etc.).
module View.Petrinet.Model.NLL where

import Prelude

import Data.Auth (Roles)
import Data.Array ((..), length, zip)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Profunctor.Strong
import Data.Set (Set, fromFoldable, toUnfoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (fst, snd, uncurry)
import Data.Foldable (foldl, fold)
import Data.Traversable (traverse)
import Data.Vec3 (Vec3, vec2)

import Data.Petrinet.Representation.Dict (mkNetObjF)
import Data.Petrinet.Representation.NLL (NetF)
import View.Petrinet.Model (PID, Transition, Typedef(..), NetRep, NetInfo, mkNetRep, mkNetApi)

toNetInfo :: NetF PID -> String -> Array String -> Array String -> Array (PID /\ Vec3 Number) -> Array (Vec3 Number) -> Array Typedef -> Array Roles -> NetInfo
toNetInfo net name placeNames transitionNames placePositions transitionPositions typedefs roles =
  { name:      name
  , net:       mkNetObjF netRep
  , textBoxes: mempty
  , netApi:    mkNetApi netRep
  }
  where
    netRep = toNetRep net placeNames transitionNames placePositions transitionPositions typedefs roles

toNetRepWithDefaults :: NetF PID -> Array String -> Array String -> Array Roles -> NetRep
toNetRepWithDefaults net placeNames transitionNames roles =
  toNetRep net placeNames transitionNames placePositions transitionPositions typedefs roles
  where
    places              = toUnfoldable $ uniquePlaceIds net
    transitions         = net

    -- lay out places and transitions in a trianglular shape
    placePositions      = (\i p -> p /\ (toNumber <$> vec2 (1+i)   (1+i)) ) `mapWithIndex` places
    transitionPositions = (\i t ->      (toNumber <$> vec2 (1+i) (-(1+i)))) `mapWithIndex` transitions

    typedefs            = Typedef "1"               <$  transitions

toNetRep :: NetF PID -> Array String -> Array String -> Array (PID /\ Vec3 Number) -> Array (Vec3 Number) -> Array Typedef -> Array Roles -> NetRep
toNetRep net placeNames transitionNames placePositions transitionPositions typedefs roles =
  mkNetRep places
           transitions
           mempty
           placeNamesIndexed
           placePositions
           transitionNames
           typedefs
           transitionPositions
           roles
  where
    places :: Array PID
    places            = toUnfoldable $ uniquePlaceIds net

    firstPlaceIndex   = 1
    numPlaces         = length places
    pids              = firstPlaceIndex .. numPlaces

    transitions       = mkTransition <$> net

    placeNamesIndexed = pids `zip` placeNames

    mkTransition :: Array PID /\ Array PID -> Transition
    mkTransition (pre /\ post) =
      { pre:  { place: _, tokens: zero } <$> pre
      , post: { place: _, tokens: zero } <$> post
      }

uniquePlaceIds :: ∀ a. Ord a => Eq a => NetF a -> Set a
uniquePlaceIds net = fromFoldable $ uncurry append =<< net
