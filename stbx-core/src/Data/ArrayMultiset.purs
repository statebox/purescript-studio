module Data.ArrayMultiset where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple.Nested (type (/\), (/\))

type ArrayMultiset = Array

-- TODO slow, may be worth optimising.
count :: ∀ a. Ord a => ArrayMultiset a -> Array (a /\ Int)
count = Map.toUnfoldable <<< countMap

-- TODO This produces a BagF data structure, pretty much.
-- TODO slow, may be worth optimising.
countMap :: ∀ a. Ord a => ArrayMultiset a -> Map a Int
countMap = Map.fromFoldableWith (+) <<< map (_ /\ 1)
