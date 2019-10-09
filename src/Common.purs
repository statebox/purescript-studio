module Common where

import Prelude

import Control.Monad.Free (Free, resume')
import Data.Array (range)
import Data.Bifunctor
import Data.Bitraversable
import Data.Traversable


data VoidF a

type Vec2 a = { x :: a, y :: a }
type Disc2 = Vec2 Int
type Cont2 = Vec2 Number

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
  foldr f z = foldrDefault f z
  foldl f z = foldlDefault f z
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
instance bifoldableAnn :: Bifoldable f => Bifoldable (Ann r f) where
  bifoldMap f g (Ann r fab) = bifoldMap f g fab
  bifoldr f g z = bifoldrDefault f g z
  bifoldl f g z = bifoldlDefault f g z
instance bitraversableAnn :: Bitraversable f => Bitraversable (Ann r f) where
  bitraverse f g (Ann r fab) = Ann r <$> bitraverse f g fab
  bisequence = bisequenceDefault

reannotateFix :: ∀ f s r a. Functor (f a) => (f a r -> r) -> Fix (Ann s f) a -> Fix (Ann r f) a
reannotateFix alg = foldFix \(Ann _ f) -> Fix (Ann (alg (getAnn <$> f)) f)

getAnn :: ∀ f r a. Fix (Ann r f) a -> r
getAnn (Fix (Ann r _)) = r

mapAccumAnn :: ∀ f a b c s. Traversable (f c) => (s -> a -> Accum s b) -> s -> Fix (Ann a f) c -> Accum s (Fix (Ann b f) c)
mapAccumAnn f = go
  where
    go s (Fix (Ann a ff)) =
      let { accum: s', value: b } = f s a in
      let { accum, value: ff' } = mapAccumL go s' ff in { accum, value: Fix (Ann b ff') }
