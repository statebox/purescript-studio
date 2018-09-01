module Data.Bag where

import Prelude
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Map (Map)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Group (class Group, ginverse) -- TODO why is Group not in Prelude? https://pursuit.purescript.org/packages/purescript-group
import Data.Tuple (Tuple)

-- | `n` is the cardinality of the element type `a`
newtype BagF a n = BagF (Map a n)

derive instance newtypeBagF :: Newtype (BagF a n)  _

derive instance functorBagF :: Functor (BagF a)

instance semigroupBagF :: (Ord a, Semigroup (Additive n)) => Semigroup (BagF a n) where
  append (BagF x) (BagF y) = BagF (Map.unionWith add x y)
    where
      add x y = unwrap $ Additive x <> Additive y

instance monoidBagF :: Semigroup (BagF a n) => Monoid (BagF a n) where
  mempty = BagF Map.empty

instance groupBagF :: (Monoid (BagF a n), Group (Additive n)) => Group (BagF a n) where
  ginverse m = un Additive <<< ginverse <<< Additive <$> m

instance showBagF :: (Show a, Show n) => Show (BagF a n) where
  show (BagF x) = "(BagF " <> show x <> ")"

--------------------------------------------------------------------------------

fromMap :: ∀ a n. Map a n -> BagF a n
fromMap = BagF

fromFoldable :: ∀ f a n. Ord a => Foldable f => f (Tuple a n) -> BagF a n
fromFoldable = BagF <<< Map.fromFoldable
