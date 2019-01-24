module View.Diagram.Common where

import Prelude
import Data.Int (toNumber, floor, round)

-- snap s x = x - (x % s)
snap :: Int -> Int -> Int
snap s x = round' (toNumber x) (toNumber s) -- s * (x // s)
  where
    round' :: Number -> Number -> Int
    round' x s = round (x / s) * (floor s)
