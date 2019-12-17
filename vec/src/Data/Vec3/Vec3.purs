module Data.Vec3.Vec3
  ( Vec3
  , vec3
  , binOp
  , _x
  , _y
  , _z
  , toArray
  , bounds
  , minMax
  , minMaxVecs
  , minMaxZero

  , Vec2()
  , vec2
  , Vec2D()
  ) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (class Newtype)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)

newtype Vec3 a = Vec3 (Vec3Rec a)

derive instance newtypeVec3 :: Newtype (Vec3 a)  _

-- | Considerations:
-- | - Has to be usable from JavaScript.
type Vec3Rec a = { x :: a, y :: a, z :: a }

-- | 'Smart' constructor.
vec3 :: forall a. a -> a -> a -> Vec3 a
vec3 x y z = Vec3 { x: x, y: y, z: z }

-- projections -----------------------------------------------------------------

_x :: ∀ a. Vec3 a -> a
_x (Vec3 {x,y,z}) = x

_y :: ∀ a. Vec3 a -> a
_y (Vec3 {x,y,z}) = y

_z :: ∀ a. Vec3 a -> a
_z (Vec3 {x,y,z}) = z

--------------------------------------------------------------------------------

binOp :: ∀ a. (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
binOp f (Vec3 { x: x1, y: y1, z: z1 })
        (Vec3 { x: x2, y: y2, z: z2 }) =
        (Vec3 { x: f x1 x2, y: f y1 y2, z: f z1 z2 })

derive instance eqVec3 :: Eq a => Eq (Vec3 a)

derive instance ordVec3 :: Ord a => Ord (Vec3 a)

instance showVec3 :: Show a => Show (Vec3 a) where
  show (Vec3 {x, y, z}) = "(" <> show x <> "," <> show y <> "," <> show z <> ")"

derive instance functorVec3 :: Functor Vec3

instance applyVec3 :: Apply Vec3 where
  apply (Vec3 {x: fx, y: fy, z: fz}) (Vec3 {x,y,z}) = Vec3 {x: fx x, y: fy y, z: fz z}

instance applicativeVec3 :: Applicative Vec3 where
  pure x = vec3 x x x

instance additiveSemigroupVec3 :: Semigroup (Additive a) => Semigroup (Vec3 (Additive a)) where
  append = binOp append

instance multiplicativeSemigroupVec3 :: Semigroup (Multiplicative a) => Semigroup (Vec3 (Multiplicative a)) where
  append = binOp append

instance additiveMonoidVec3 :: (Semigroup (Vec3 (Additive a)), Monoid (Additive a)) => Monoid (Vec3 (Additive a)) where
  mempty = pure mempty

instance multiplicativeMonoidVec3 :: (Semigroup (Vec3 (Multiplicative a)), Monoid (Multiplicative a)) => Monoid (Vec3 (Multiplicative a)) where
  mempty = pure mempty

instance semiringVec3 :: Semiring a => Semiring (Vec3 a) where
  zero = pure zero
  add  = binOp (+)
  one  = pure one
  mul  = binOp (*)

instance ringVec3 :: (Ring a, Semiring (Vec3 a)) => Ring (Vec3 a) where
  sub  = binOp (-)

instance commutativeRingVec3 :: CommutativeRing a => CommutativeRing (Vec3 a)

instance divisionRingVec3 :: DivisionRing a => DivisionRing (Vec3 a) where
  recip = map recip

instance euclideanRingVec3 :: EuclideanRing a => EuclideanRing (Vec3 a) where
  degree = degree <<< _x
  div    = binOp div
  mod    = binOp mod

instance minSemigroupVec3 :: Semigroup (Min a) => Semigroup (Vec3 (Min a)) where
  append = binOp append

instance maxSemigroupVec3 :: Semigroup (Max a) => Semigroup (Vec3 (Max a)) where
  append = binOp append

instance minMonoidVec3 :: (Semigroup (Vec3 (Min a)), Monoid (Min a)) => Monoid (Vec3 (Min a)) where
  mempty = pure mempty

instance maxMonoidVec3 :: (Semigroup (Vec3 (Max a)), Monoid (Max a)) => Monoid (Vec3 (Max a)) where
  mempty = pure mempty

bounds :: ∀ f a. Bounded a => Ord a => Foldable f => f (Vec3 a) -> { min :: Vec3 a, max :: Vec3 a }
bounds vecs =
  foldl minMaxVecs minMaxZero vecs

minMaxZero :: ∀ a. Bounded a => { min :: Vec3 a, max :: Vec3 a }
minMaxZero = { min: pure top, max: pure bottom }

minMaxVecs :: ∀ a. Bounded a => Ord a => { min :: Vec3 a, max :: Vec3 a } -> Vec3 a -> { min :: Vec3 a, max :: Vec3 a }
minMaxVecs { min: vMin, max: vMax } v = { min: min <$> vMin <*> v, max: max <$> vMax <*> v }

-- this would be something like the monoidal (append) operation on the {min, max} record of (Vec3 a) type
minMax
  :: ∀ a
   . Bounded a
  => Ord a
  => { min :: Vec3 a, max :: Vec3 a }
  -> { min :: Vec3 a, max :: Vec3 a }
  -> { min :: Vec3 a, max :: Vec3 a }
minMax m n =
  { min: binOp min m.min n.min
  , max: binOp max m.max n.max
  }

toArray :: forall a. Vec3 a -> Array a
toArray v = [_x v, _y v, _z v]

-- Legacy Vec2 interface--------------------------------------------------------

type Vec2 a = Vec3 a

type Vec2D = Vec3 Number

-- | 'Smart' constructor.
vec2 :: ∀ a. Semiring a => a -> a -> Vec3 a
vec2 x y = vec3 x y zero
