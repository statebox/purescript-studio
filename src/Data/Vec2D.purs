module Data.Vec2D where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Newtype (class Newtype, un)
import Data.Ring (negate)
import Data.Semiring

newtype Vec2 a = Vec2 (Vec2Rec a)

type Vec2Rec a = { x :: a, y :: a }

type Vec2D = Vec2Rec Number

derive instance newtypeVec2 :: Newtype (Vec2 a)  _

-- | Convenience constructor.
vec2 :: forall a. a -> a -> Vec2 a
vec2 x y = Vec2 { x: x , y: y }

scalarMulVec2D :: Number -> Vec2D -> Vec2D
scalarMulVec2D a {x, y} = { x: a*x, y: a*y }

zeroVec2 :: forall a. Semiring a => Vec2 a
zeroVec2 = Vec2 { x: zero, y: zero }

addVec2 :: forall a. Semiring a => Vec2 a -> Vec2 a -> Vec2 a
addVec2 (Vec2 { x: x1, y: y1 }) (Vec2 { x: x2, y: y2 }) = Vec2 { x: x1 + x2, y: y1 + y2 }

subVec2 :: forall a. Ring a => Vec2 a -> Vec2 a -> Vec2 a
subVec2 (Vec2 { x: x1, y: y1 }) (Vec2 { x: x2, y: y2 }) = Vec2 { x: x1 - x2, y: y1 - y2 }

oneVec2 :: forall a. Semiring a => Vec2 a
oneVec2 = Vec2 { x: zero, y: zero }

mulVec2 :: forall a. Semiring a => Vec2 a -> Vec2 a -> Vec2 a
mulVec2 (Vec2 { x: x1, y: y1 }) (Vec2 { x: x2, y: y2 }) = Vec2 { x: x1 * x2, y: y1 * y2 }

instance semiringVec :: Semiring a => Semiring (Vec2 a) where
  zero = zeroVec2
  add  = addVec2
  mul  = mulVec2
  one  = oneVec2

instance ringVec :: (Ring a, Semiring (Vec2 a)) => Ring (Vec2 a) where
  sub  = subVec2

-- minMax2 :: { min :: Vec2D, max :: Vec2D }
--         -> { min :: Vec2D, max :: Vec2D }
--         -> { min :: Vec2D, max :: Vec2D }
-- minMax2 { min: vMin, max: vMax }
--         { min: wMin, max: wMax }
--       = { min: minCoord vMin wMin
--         , max: maxCoord vMax wMax
--         }


-- TODO monoid instance? bifunctor? tuple?

-- TODO ehh mss kan dit handiger met tuple of eoa vector impl? mss een `iso`tje ertegenaanknallen
-- TODO en sws Vec2D heeft natuurlijk geen instances etc maar is basically een gespecialiseerde tuple; mss ff libje pakken instead
-- TODO hmm :/ voor efficientie doen we hier min en max door elkaar
-- TODO shit, this is one shitload of reimplementations; monoidal stuff, bifunctor stuff, etc
-- TODO we werken hier toevallig met de dicts in net, maar het mogen (liever, want algemener) ook lijsten of foldables zijn

bounds :: ∀ f. Foldable f => f Vec2D -> { min :: Vec2D, max :: Vec2D }
bounds vecs =
  foldl minMaxVecs1 minMaxZero vecs
  where
    minMaxVecs1 :: { min :: Vec2D, max :: Vec2D } -> Vec2D -> { min :: Vec2D, max :: Vec2D }
    minMaxVecs1 { min: vMin, max: vMax } v =
      { min: minCoord vMin v
      , max: maxCoord vMax v
      }

    -- TODO MIN_FLOAT and MAX_FLOAT or whatevs; maybe in Global?
    minMaxZero =
      { min: {x:  999999.9, y:  999999.9}
      , max: {x: -999999.9, y: -999999.9}
      }

    maxCoord {x:x1, y:y1} {x:x2, y:y2} = {x: x1 `max` x2, y: y1 `max` y2}
    minCoord {x:x1, y:y1} {x:x2, y:y2} = {x: x1 `min` x2, y: y1 `min` y2}

--------------------------------------------------------------------------------

newtype Box a = Box (BoxRec a)

derive instance newtypeBox :: Newtype (Box a)  _

type BoxRec a =
  { topLeft     :: Vec2 a
  , bottomRight :: Vec2 a
  }

-- TODO rename (its not a norm or anything but whatsit called?)
wh :: forall a. Ring a => Box a -> Vec2 a
wh (Box box) = box.bottomRight - box.topLeft
