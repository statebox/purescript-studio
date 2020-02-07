module Statebox.Core.Marking
  ( Tokens
  , PlaceMarkingF
  , TransitionF
  , buildTransitionMarking
  , buildPlaceMarkings
  ) where

import Prelude
import Data.Array ((:))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)

import Data.ArrayMultiset as ArrayMultiset
import Data.ArrayMultiset (ArrayMultiset)

--------------------------------------------------------------------------------

-- | TODO Duplicated from `Data.Petrinet.Representation.Dict` in #328.
type Tokens = Int

-- | TODO Duplicated from `Data.Petrinet.Representation.Dict` in #328.
type TransitionF p tok =
  { pre  :: Array (PlaceMarkingF p tok)
  , post :: Array (PlaceMarkingF p tok)
  }

-- | TODO Duplicated from `Data.Petrinet.Representation.Dict` in #328.
type PlaceMarkingF p tok =
  { place  :: p
  , tokens :: tok
  }

--------------------------------------------------------------------------------

buildTransitionMarking :: ∀ a. Ord a => ArrayMultiset a -> ArrayMultiset a -> TransitionF a Tokens
buildTransitionMarking pre post =
  { pre:  buildPlaceMarkings pre
  , post: buildPlaceMarkings post
  }

buildPlaceMarkings :: ∀ a. Ord a => ArrayMultiset a -> Array (PlaceMarkingF a Tokens)
buildPlaceMarkings netMarking =
  foldrWithIndex (\place tokens -> (:) { place, tokens }) [] netMarkingDict
  where
    netMarkingDict :: Map a Tokens
    netMarkingDict = ArrayMultiset.countMap netMarking
