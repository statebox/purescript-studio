module Data.Petrinet.Representation.NLL where

import Prelude
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Bitraversable
import Data.Either (Either(..), hush)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.NLL (mapWindow2, splitOn)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable

-- | A Petri net encoding based on https://github.com/statebox/stbx-js/blob/master/spec/net-encoding.png
type NetF p = Array (Array p /\ Array p)

data ErrNetEncoding = ErrOddLength

instance showErrNetEncoding :: Show ErrNetEncoding where
  show ErrOddLength = "ErrOddLength"

derive instance eqErrNetEncoding :: Eq ErrNetEncoding

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
