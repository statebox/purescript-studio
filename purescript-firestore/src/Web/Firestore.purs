module Web.Firestore where

import Data.Function.Uncurried (Fn1)

foreign import firestore :: forall a b. Fn1 a b