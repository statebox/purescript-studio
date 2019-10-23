module Data.Petrinet.Representation.Layout.Dagre where

import Prelude
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Vec3 (Vec3, vec2)
import Foreign.Object (Object, lookup)

import Data.Petrinet.Representation.Dict (NetRepF, computeTransitionIds)
import Data.Petrinet.Representation.Layout (NetLayoutF)

type DagreInput = 
  { places      :: Array String
  , transitions :: Array String
  , edges       :: Array { source :: String, target :: String }
  }

type DagreOutput = 
  { places      :: Object { x :: Number, y :: Number }
  , transitions :: Object { x :: Number, y :: Number }
  }

foreign import dagreLayout :: DagreInput -> DagreOutput

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
  }
  where
    output = dagreLayout input
    input = 
      { places: net.places <#> show
      , transitions: tids <#> show
      , edges: net.transitionsDict # foldMapWithIndex \tid { pre, post } -> 
          map (\{ place } -> { source: show place, target: show tid   }) pre <>
          map (\{ place } -> { source: show tid  , target: show place }) post
      }
    tids = computeTransitionIds net
    toMap :: ∀ id. Show id => Ord id => Array id -> Object { x :: Number, y :: Number } -> Map.Map id (Vec3 Number)
    toMap ids points = ids # foldMap \i -> lookup (show i) points # foldMap \{x, y} -> Map.singleton i (vec2 (x / 4.0) (y / 4.0))
