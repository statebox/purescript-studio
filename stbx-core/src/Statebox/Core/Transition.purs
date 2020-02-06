module Statebox.Core.Transition where

import Prelude
import Data.Array ((:))
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))

import Data.ArrayMultiset (ArrayMultiset)
import Statebox.Core.Execution (Path)
import Statebox.Core.Types (PID, TID)

--------------------------------------------------------------------------------

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

-- | TODO Duplicated from `Data.Petrinet.Representation.Dict` in #328.
type Tokens = Int

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
    netMarkingDict = netMarking # foldr (Map.update (Just <<< (_ + 1))) mempty

--------------------------------------------------------------------------------

type Transition =
  { path       :: Path
  , transition :: TID
  , name       :: String
  , tokens     :: TransitionF PID Tokens
  }

data Glued a
  = Untouched a
  | Initial a
  | Final a
  | Glued a a

isInitial :: ∀ a. Glued a -> Boolean
isInitial = case _ of
  Initial a -> true
  _         -> false

isFinal :: ∀ a. Glued a -> Boolean
isFinal = case _ of
  Final a -> true
  _       -> false

gluedTokens :: Glued Transition -> TransitionF PID Tokens
gluedTokens = case _ of
  Untouched transition              -> transition.tokens
  Initial   transition              -> transition.tokens
  Final     transition              -> transition.tokens
  Glued     transition1 transition2 -> { pre:  transition1.tokens.pre
                                       , post: transition2.tokens.post
                                       }
