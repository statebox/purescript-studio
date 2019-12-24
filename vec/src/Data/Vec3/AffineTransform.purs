module Data.Vec3.AffineTransform where

import Prelude
import Data.Newtype (class Newtype)
import Math (cos, sin, Radians)

import Data.Vec3.Vec3

-- A 3x3 matrix with only 2 rows given, the 3rd is always [0 0 1]
newtype AffineTransform a = AF (AffineTransformF (Vec3 a))

type AffineTransformF a =
  { x :: a
  , y :: a
  }

extended :: ∀ a. Semiring a => AffineTransform a -> Vec3 (Vec3 a)
extended (AF {x, y}) = vec3 x y (vec3 zero zero one)

derive instance newtypeBox :: Newtype (AffineTransform a) _
derive instance functorBox :: Functor AffineTransform
derive instance eqBox :: Eq a => Eq (AffineTransform a)
derive instance ordBox :: Ord a => Ord (AffineTransform a)

transform :: ∀ a. Semiring a => AffineTransform a -> Vec3 a -> Vec3 a
transform (AF { x, y }) v = vec3 (x `inproduct` v) (y `inproduct` v) (_z v)

scale :: ∀ a. Semiring a => a -> AffineTransform a
scale s = AF
  { x: vec2 s zero
  , y: vec2 zero s
  }

translate :: ∀ a. Semiring a => Vec2 a -> AffineTransform a
translate v = AF
  { x: vec3 zero zero (_x v)
  , y: vec3 zero zero (_y v)
  }

rotate :: Radians -> AffineTransform Number
rotate a = AF
  { x: vec2 c s
  , y: vec2 (-s) c
  }
  where
    c = cos a
    s = sin a

instance semiringVec3 :: Semiring a => Semiring (AffineTransform a) where
  zero = scale zero
  add (AF l) (AF r) = AF { x: l.x + r.x, y: l.y + r.y }
  one = scale one
  --                     [r.x.x r.x.y r.x.z]
  --                     [r.y.x r.y.y r.y.z]
  --                     [    0     0     1]
  -- [l.x.x l.x.y l.x.z]
  -- [l.y.x l.y.y l.y.z]
  mul (AF l) r = AF
    { x: map pure l.x `inproduct` extendedR
    , y: map pure l.y `inproduct` extendedR
    }
    where
      extendedR = extended r
