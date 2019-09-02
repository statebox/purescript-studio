-- | Convert NLL-encoded Petri nets to view models (`NetInfo`, `NetRep`, etc.).
module View.Petrinet.Model.NLL where

import Prelude

import Data.Auth (Roles)
import Data.Array ((..), length, zip)
import Data.ArrayMultiset (ArrayMultiset)
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

import Data.Petrinet.Representation.NLL (NetF)
import Data.Typedef (Typedef(..))
import View.Petrinet.Model (PID, Transition, NetRep, NetInfo, TextBox, mkNetRep, mkNetApi, mkNetInfo)

toNetInfo :: NetF PID -> String -> Array String -> Array String -> Array (PID /\ Vec3 Number) -> Array (Vec3 Number) -> Array Typedef -> Array Roles -> NetInfo
toNetInfo net name placeNames transitionNames placePositions transitionPositions typedefs roles =
  mkNetInfo netRep name mempty
  where
    netRep = toNetRep net placeNames transitionNames placePositions transitionPositions typedefs roles

toNetInfoWithDefaults :: NetF PID -> String -> Array String -> Array String -> NetInfo
toNetInfoWithDefaults net name placeNames transitionNames =
  mkNetInfo netRep name mempty
  where
    netRep = toNetRepWithDefaults net placeNames transitionNames

toNetRepWithDefaults :: NetF PID -> Array String -> Array String -> NetRep
toNetRepWithDefaults net placeNames transitionNames =
  toNetRep net placeNames transitionNames layout.placePositions layout.transitionPositions typedefs roles
  where
    transitions = net
    typedefs    = Typedef "1" <$ transitions
    roles       = mempty
    layout      = layoutBipartiteIn2ColsNetF net

toNetRep :: NetF PID -> Array String -> Array String -> Array (PID /\ Vec3 Number) -> Array (Vec3 Number) -> Array Typedef -> Array Roles -> NetRep
toNetRep net placeNames transitionNames placePositions transitionPositions typedefs roles =
  mkNetRep places
           transitions
           mempty
           placeNamesIndexed
           (pure placePositions)
           transitionNames
           typedefs
           (pure transitionPositions)
           roles
  where
    places            :: Array PID
    places            = toUnfoldable $ uniquePlaceIds net

    firstPlaceIndex   = 1
    numPlaces         = length places
    pids              = firstPlaceIndex .. numPlaces

    transitions       = mkTransition <$> net

    placeNamesIndexed = pids `zip` placeNames

    mkTransition :: ArrayMultiset PID /\ ArrayMultiset PID -> Transition
    mkTransition (pre /\ post) =
      { pre:  { place: _, tokens: zero } <$> pre
      , post: { place: _, tokens: zero } <$> post
      }

--------------------------------------------------------------------------------

layoutBipartiteIn2ColsNetF :: NetF PID -> { placePositions :: Array (PID /\ Vec3 Number), transitionPositions :: Array (Vec3 Number) }
layoutBipartiteIn2ColsNetF net =
  { placePositions:      (\i p -> p /\ vec2 (-halfWidth) (scale <<< toNumber $ 1+i)) `mapWithIndex` places
  , transitionPositions: (\i t ->      vec2   halfWidth  (scale <<< toNumber $ 1+i)) `mapWithIndex` net
  }
  where
    places    = toUnfoldable $ uniquePlaceIds net
    halfWidth = scale <<< (_ / 2.0) <<< toNumber $ maxIndex -- maxIndex determines height
    maxIndex  = max (length places) (length net)

scale :: Number -> Number
scale n = 10.0*n

defaultPlaceNames :: ∀ a. Ord a => Eq a => NetF a -> Array String
defaultPlaceNames net = defaultPlaceNames' numPlaces
  where
    numPlaces = length $ toUnfoldable $ uniquePlaceIds net

defaultPlaceNames' :: Int -> Array String
defaultPlaceNames' numPlaces = (append "p" <<< show) <$> (0 .. (numPlaces-1))

-- TODO it seems this function gets called a lot, which is unnecessarily costly, given strictness
uniquePlaceIds :: ∀ a. Ord a => Eq a => NetF a -> Set a
uniquePlaceIds net = fromFoldable $ uncurry append =<< net
