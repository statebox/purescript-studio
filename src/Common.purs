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