module Common where

import Prelude

import Control.Monad.Free (Free, resume')
import Data.Array (range)
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Traversable


data VoidF a


rangeEx :: Int -> Int -> Array Int
rangeEx x y = if y > x then range x (y - 1) else []

infix 8 rangeEx as ..<


foldFree :: ∀ a f r. Functor f => (f r -> r) -> (a -> r) -> Free f a -> r
foldFree f a = go where go = resume' (\fb bf -> f $ (go <<< bf) <$> fb) a


newtype Fix f a = Fix (f a (Fix f a))

instance functorFix :: Bifunctor f => Functor (Fix f) where
  map f (Fix ff) = Fix (bimap f (map f) ff)
instance foldableFix :: Bifoldable f => Foldable (Fix f) where
  foldMap f (Fix ff) = bifoldMap f (foldMap f) ff
  foldr = foldrDefault
  foldl = foldlDefault
instance traverseFix :: Bitraversable f => Traversable (Fix f) where
  traverse f (Fix ff) = Fix <$> bitraverse f (traverse f) ff
  sequence = sequenceDefault

foldFix :: ∀ f r a. Functor (f a) => (f a r -> r) -> Fix f a -> r
foldFix alg = go where go (Fix ff) = alg (go <$> ff)

data Ann r f a b = Ann r (f a b)
instance functorAnn :: Functor (f a) => Functor (Ann r f a) where
  map f (Ann r fab) = Ann r (map f fab)
instance bifunctorAnn :: Bifunctor f => Bifunctor (Ann r f) where
  bimap f g (Ann r fab) = Ann r (bimap f g fab)

annotateFix :: ∀ f r a. Functor (f a) => (f a r -> r) -> Fix f a -> Fix (Ann r f) a
annotateFix alg = foldFix \f -> Fix (Ann (alg (getAnn <$> f)) f)

getAnn :: ∀ f r a. Fix (Ann r f) a -> r
getAnn (Fix (Ann r _)) = r

mapAnn :: ∀ f a b c. Functor (f c) => (a -> b) -> Fix (Ann a f) c -> Fix (Ann b f) c
mapAnn f = go where go (Fix (Ann a ff)) = Fix (Ann (f a) (go <$> ff))
