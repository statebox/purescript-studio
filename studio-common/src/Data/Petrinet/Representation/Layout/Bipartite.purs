module Data.Petrinet.Representation.Layout.Bipartite where

import Prelude
import Data.Array (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Vec3 (vec2)

import Data.Petrinet.Representation.Dict (NetRepF)
import Data.Petrinet.Representation.Layout (NetLayoutF)

bipartite :: ∀ pid tid tok ty r. Ord pid => Ord tid => NetRepF pid tid tok ty r -> NetLayoutF pid tid
bipartite net =
  { placePointsDict:      Map.fromFoldable placePoints
  , transitionPointsDict: Map.fromFoldable transitionPoints
  }
  where
    placePoints      = net.places # mapWithIndex (\i pid -> pid /\ vec2 xLeft  ((_ - halfHeight) <<< scale $ toNumber $ 1+i))
    transitionPoints = tids       # mapWithIndex (\i tid -> tid /\ vec2 xRight ((_ - halfHeight) <<< scale $ toNumber $ 1+i))

    xLeft            = -halfWidth
    xRight           =  halfWidth
    halfHeight       = scale <<< (_ / 2.0) <<< toNumber $ maxIndex -- maxIndex determines height
    halfWidth        = halfHeight -- TODO improve this

    maxIndex         = numTransitions `max` length net.places

    numTransitions = length tids

    tids :: Array tid
    tids = Set.toUnfoldable $ Map.keys net.transitionLabelsDict

scale :: Number -> Number
scale n = 20.0*n
