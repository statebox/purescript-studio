module Data.Bag where

import Prelude
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Group (class Group, ginverse) -- TODO why is Group not in Prelude? https://pursuit.purescript.org/packages/purescript-group
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)

-- | A bag or multiset where `n` is the multiplicity of the element type `a`.
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

instance eqBagF :: (Eq a, Eq n) => Eq (BagF a n) where
  eq (BagF x) (BagF y) = eq x y

--------------------------------------------------------------------------------

fromMap :: ∀ a n. Map a n -> BagF a n
fromMap = BagF

toMap :: ∀ a b. BagF a b -> Map a b
toMap (BagF dict) = dict

fromFoldable :: ∀ f a n. Ord a => Foldable f => f (Tuple a n) -> BagF a n
fromFoldable = BagF <<< Map.fromFoldable

toUnfoldable :: ∀ f a n. Unfoldable f => BagF a n -> f (Tuple a n)
toUnfoldable (BagF b) = Map.toUnfoldable b

lookup :: ∀ a n. Ord a => a -> BagF a n -> Maybe n
lookup k (BagF m) = Map.lookup k m

lookup' :: ∀ a n. Ord a => BagF a n -> a -> Maybe n
lookup' = flip lookup
