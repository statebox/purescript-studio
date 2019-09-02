module Data.Petrinet.Representation.Layout where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Vec3 as Vec3
import Data.Vec3 (Vec3)

type NetLayoutFRow pid tid r =
  ( placePointsDict      :: Map pid (Vec3 Number)
  , transitionPointsDict :: Map tid (Vec3 Number)
  | r
  )

type NetLayoutF pid tid r = Record (NetLayoutFRow pid tid r)

mapVec2D
  :: forall pid tid r
   . (Vec3 Number -> Vec3 Number)
  -> NetLayoutF pid tid r
  -> NetLayoutF pid tid r
mapVec2D f l =
  l { placePointsDict      = f <$> l.placePointsDict
    , transitionPointsDict = f <$> l.transitionPointsDict
    }

bounds :: ∀ pid tid r. NetLayoutF pid tid r -> { min :: Vec3 Number, max :: Vec3 Number }
bounds layout =
  Vec3.bounds $
    Map.values layout.placePointsDict <>
    Map.values layout.transitionPointsDict

