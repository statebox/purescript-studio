module Common where

import Prelude

import Control.Monad.Free (Free, resume')
import Data.Array (range)


data VoidF a


rangeEx :: Int -> Int -> Array Int
rangeEx x y = if y > x then range x (y - 1) else []

infix 8 rangeEx as ..<


foldFree :: ∀ a f r. Functor f => (f r -> r) -> (a -> r) -> Free f a -> r
foldFree f a = go where go = resume' (\fb bf -> f $ (go <<< bf) <$> fb) a


newtype Fix f = Fix (f (Fix f))

foldFix :: ∀ f r. Functor f => (f r -> r) -> Fix f -> r
foldFix alg = go where go (Fix ff) = alg (go <$> ff)

data Ann r f a = Ann r (f a)
instance functorAnn :: Functor f => Functor (Ann r f) where
  map f (Ann r fa) = Ann r (map f fa)

annotateFix :: ∀ f r. Functor f => (f r -> r) -> Fix f -> Fix (Ann r f)
annotateFix alg = foldFix \f -> Fix (Ann (alg (getAnn <$> f)) f)

getAnn :: ∀ f r. Fix (Ann r f) -> r
getAnn (Fix (Ann r _)) = r

mapAnn :: ∀ f a b. Functor f => (a -> b) -> Fix (Ann a f) -> Fix (Ann b f)
mapAnn f = go where go (Fix (Ann a ff)) = Fix (Ann (f a) (go <$> ff))
