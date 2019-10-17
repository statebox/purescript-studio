module View.Diagram.Common where

import Prelude
import Data.Int (toNumber, round)

-- snap s x = x - (x % s)
snap :: Int -> Int -> Int
snap s x = s * round (toNumber x / toNumber s) -- s * (x // s)
