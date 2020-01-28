module Data.Petrinet.Representation.Layout.Dagre where

import Prelude
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Vec3 (Vec3, vec2)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object, lookup)

import Data.Petrinet.Representation.Dict (NetRepF, computeTransitionIds)
import Data.Petrinet.Representation.Layout (NetLayoutF)

type DagreInput pid tid =
  { places      :: Array String
  , transitions :: Array String
  , edges       :: Array { tid :: tid, place :: pid, source :: String, target :: String }
  }

type DagreOutput pid tid =
  { places      :: Object { x :: Number, y :: Number }
  , transitions :: Object { x :: Number, y :: Number }
  , edges       :: Array { tid :: tid, place :: pid, waypoints :: Array { x :: Number, y :: Number } }
  }

foreign import dagreLayout :: ∀ pid tid. DagreInput pid tid -> DagreOutput pid tid

layout
  :: ∀ pid tid tok ty r
   . Ord pid
  => Show pid
  => Ord tid
  => Show tid
  => NetRepF pid tid tok ty r
  -> NetLayoutF pid tid
layout net =
  { placePointsDict:      toMap net.places output.places
  , transitionPointsDict: toMap tids output.transitions
  , edgeWaypointsDict:    output.edges # foldMap \{ tid, place, waypoints } -> Map.singleton (place /\ tid) (map toVec3 waypoints)
  }
  where
    output = dagreLayout input
    input =
      { places: net.places <#> show
      , transitions: tids <#> show
      , edges: net.transitionsDict # foldMapWithIndex \tid { pre, post } ->
          map (\{ place } -> { source: show place, target: show tid  , tid, place }) pre <>
          map (\{ place } -> { source: show tid  , target: show place, tid, place }) post
      }
    tids = computeTransitionIds net

    toMap :: ∀ id. Show id => Ord id => Array id -> Object { x :: Number, y :: Number } -> Map id (Vec3 Number)
    toMap ids points = ids # foldMap \i -> lookup (show i) points # foldMap \p -> Map.singleton i (toVec3 p)

    toVec3 :: { x :: Number, y :: Number } -> Vec3 Number
    toVec3 {x, y} = vec2 (x / 4.0) (y / 4.0)
