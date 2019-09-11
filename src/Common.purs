module Common where

import Prelude

import Data.Array (range)


data VoidF a

rangeEx :: Int -> Int -> Array Int
rangeEx x y = if y > x then range x (y - 1) else []

infix 8 rangeEx as ..<
