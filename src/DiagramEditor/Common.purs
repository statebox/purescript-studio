module DiagramEditor.Common where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.Int (toNumber, floor, round)

-- snap s x = x - (x % s)
snap :: Int -> Int -> Int
snap s x = round' (toNumber x) (toNumber s) -- s * (x // s)
  where
    round' :: Number -> Number -> Int
    round' x s = round (x / s) * (floor s)

-- TODO replace by subtraction on vectors
dxdy :: (Int /\ Int) -> (Int /\ Int) -> (Int /\ Int)
dxdy (x1 /\ y1) (x2 /\ y2) = ((x1 - x2) /\ (y1 - y2))

showVec2 :: ∀ a. Show a => a /\ a -> String
showVec2 (x /\ y) = "(" <> show x <> "," <> show y <> ")"

showVec3 :: ∀ a. Show a => (a /\ a /\ a) -> String
showVec3 (x /\ y /\ z) = "(" <> show x <> "," <> show y <> "," <> show z <> ")"
