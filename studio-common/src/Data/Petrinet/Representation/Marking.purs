module Data.Petrinet.Representation.Marking
  ( MarkingF
  , fromFoldable
  , fromMap
  , toMap
  , toUnfoldable
  , tokensAt
  , findTokens
  ) where

import Prelude
import Data.Foldable (class Foldable, foldMap)
import Data.Unfoldable (class Unfoldable)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)
import Data.Tuple.Nested (type (/\))
import Data.Bag (BagF(..))
import Data.Bag as Bag

type MarkingF a n = BagF a n

fromFoldable :: ∀ f a b. Ord a => Foldable f => f (a /\ b) -> BagF a b
fromFoldable = Bag.fromFoldable

toUnfoldable :: ∀ f a b. Unfoldable f => BagF a b -> f (a /\ b)
toUnfoldable = Bag.toUnfoldable

fromMap :: ∀ a b. Map a b -> BagF a b
fromMap = BagF

toMap :: ∀ a b. BagF a b -> Map a b
toMap (BagF dict) = dict

tokensAt :: forall a n. Ord a => BagF a n -> a -> Maybe n
tokensAt = Bag.lookup'

--------------------------------------------------------------------------------

findTokens
  :: ∀ p tok
   . Ord p
  => Monoid (Additive tok)
  => MarkingF p tok
  -> p
  -> tok
findTokens marking place = ala Additive foldMap $ marking `tokensAt` place
