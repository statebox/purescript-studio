module Data.Petrinet.Representation.Layout where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple.Nested (type (/\))
import Data.Vec3 as Vec3
import Data.Vec3 (Vec3)

type NetLayoutF pid tid = Record (NetLayoutFRow pid tid ())

type NetLayoutFRow pid tid r =
  ( placePointsDict      :: Map pid (Vec3 Number)
  , transitionPointsDict :: Map tid (Vec3 Number)
  , edgeWaypointsDict    :: Map (pid /\ tid) (Array (Vec3 Number))
  | r
  )

mapVec2D
  :: ∀ pid tid
   . (Vec3 Number -> Vec3 Number)
  -> NetLayoutF pid tid
  -> NetLayoutF pid tid
mapVec2D f l =
  l { placePointsDict      = f <$> l.placePointsDict
    , transitionPointsDict = f <$> l.transitionPointsDict
    , edgeWaypointsDict    = map f <$> l.edgeWaypointsDict
    }

bounds :: ∀ pid tid. NetLayoutF pid tid -> { min :: Vec3 Number, max :: Vec3 Number }
bounds layout = Vec3.bounds (Map.values layout.placePointsDict <> Map.values layout.transitionPointsDict)
