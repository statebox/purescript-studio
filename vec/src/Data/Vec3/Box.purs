module Data.Vec3.Box
  ( Box(..)
  , BoxRecF
  , mapVec3s
  , toArray
  ) where

import Prelude
import Data.Newtype (class Newtype, over)

import Data.Vec3.Vec3

newtype Box a = Box (BoxRecF (Vec3 a))

derive instance newtypeBox :: Newtype (Box a)  _
derive instance functorBox :: Functor Box
derive instance eqBox :: Eq a => Eq (Box a)
derive instance ordBox :: Ord a => Ord (Box a)

type BoxRecF a =
  { topLeft     :: a
  , bottomRight :: a
  }

map :: ∀ a b. (a -> b) -> BoxRecF a -> BoxRecF b
map f {topLeft, bottomRight} = {topLeft: f topLeft, bottomRight: f bottomRight}

mapVec3s :: ∀ a b. (Vec3 a -> Vec3 b) -> Box a -> Box b
mapVec3s f b = over Box (map f) b

toArray :: ∀ a. Box a -> Array (Vec3 a)
toArray (Box {topLeft, bottomRight}) = [topLeft, bottomRight]
