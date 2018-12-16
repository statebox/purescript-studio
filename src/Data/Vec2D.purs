module Data.Vec2D where

import Prelude (min, max)
import Data.Ring (negate)
import Data.Foldable (class Foldable, foldl)
import Data.Semiring

type Vec2D = { x :: Number, y :: Number }

type Vec4D =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }
  
newtype Vec2 a = Vec2 { x :: a, y :: a }

addVec :: forall a. Semiring a => Vec2 a -> Vec2 a -> Vec2 a
addVec (Vec2 { x: x1, y: y1 }) (Vec2 { x: x2, y: y2 }) = Vec2 { x: x1 + x2, y: y1 + y2 }

mulVec :: forall a. Semiring a => Vec2 a -> Vec2 a -> Vec2 a
mulVec (Vec2 { x: x1, y: y1 }) (Vec2 { x: x2, y: y2 }) = Vec2 { x: x1 * x2, y: y1 * y2 }

zeroVec :: forall a. Semiring a => Vec2 a
zeroVec = Vec2 { x: zero, y: zero }

oneVec :: forall a. Semiring a => Vec2 a
oneVec = Vec2 { x: zero, y: zero }

instance semiringVec :: Semiring a => Semiring (Vec2 a) where
  zero = zeroVec
  add = addVec
  mul = mulVec
  one = oneVec

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
