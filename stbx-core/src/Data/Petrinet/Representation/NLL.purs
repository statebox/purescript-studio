-- | A Petri net encoding based on NLL (Number List List).
-- |
-- | See https://github.com/statebox/stbx-js/blob/master/spec for more info.
-- | Details on NLL are here: https://github.com/statebox/nll-spec.
module Data.Petrinet.Representation.NLL where

import Prelude
import Data.Array (length)
import Data.ArrayMultiset (ArrayMultiset)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), hush)
import Data.Int (even)
import Data.Maybe (Maybe)
import Data.NLL (mapWindow2, splitOn)
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\))
import Data.Traversable (traverse)

-- | A Petri net encoding based on NLL (Number List List).
-- | See https://github.com/statebox/stbx-js/blob/master/spec for more info.
type NetF p = Array (TransitionF' p)

-- TODO Come up with a name that isn't absolutely horrific.
type TransitionF' p = ArrayMultiset p /\ ArrayMultiset p

--------------------------------------------------------------------------------

data ErrNetEncoding = ErrOddLength

instance showErrNetEncoding :: Show ErrNetEncoding where
  show ErrOddLength = "ErrOddLength"

derive instance eqErrNetEncoding :: Eq ErrNetEncoding

--------------------------------------------------------------------------------

mapNetF :: forall a b. (a -> b) -> NetF a -> NetF b
mapNetF f = map (bimap (map f) (map f))

traverseNetF :: forall f a b. Applicative f => (a -> f b) -> NetF a -> f (NetF b)
traverseNetF f fx = traverse (bitraverse (traverse f) (traverse f)) fx

fromNLL :: forall a. Eq a => a -> Array a -> Either ErrNetEncoding (NetF a)
fromNLL separator xs =
  if even (length ys)
  then Right <<< mapWindow2 Tuple $ ys
  else Left ErrOddLength
  where
    ys = splitOn separator xs

fromNLLMaybe :: forall a. Eq a => a -> Array a -> Maybe (NetF a)
fromNLLMaybe separator = hush <<< fromNLL separator

--------------------------------------------------------------------------------

-- TODO it seems this function gets called a lot, which is (unnecessarily?) costly b/c of strictness
uniquePlaceIds :: ∀ a. Ord a => Eq a => NetF a -> Set a
uniquePlaceIds net = Set.fromFoldable $ uncurry append =<< net
