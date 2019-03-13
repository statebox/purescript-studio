module Data.Vec3.Box
  ( Box(..)
  , BoxRec
  , mapVec3s
  , toArray
  ) where

import Prelude
import Data.Newtype (class Newtype)

import Data.Vec3.Vec3

newtype Box a = Box (BoxRec a)

derive instance newtypeBox :: Newtype (Box a)  _
derive instance functorBox :: Functor Box

type BoxRec a =
  { topLeft     :: Vec3 a
  , bottomRight :: Vec3 a
  }

mapVec3s :: ∀ a. (Vec3 a -> Vec3 a) -> Box a -> Box a
mapVec3s f (Box {topLeft, bottomRight}) =
  Box {topLeft: f topLeft, bottomRight: f bottomRight}

toArray :: ∀ a. Box a -> Array (Vec3 a)
toArray (Box {topLeft, bottomRight}) = [topLeft, bottomRight]
