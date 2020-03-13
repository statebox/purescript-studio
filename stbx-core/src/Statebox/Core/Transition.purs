module Statebox.Core.Transition where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)

import Statebox.Core.Marking (Tokens, TransitionF)
import Statebox.Core.Types (PID, TID)
import Statebox.Core.Wiring.Tree (Path)

type Transition =
  { path       :: Path
  , transition :: TID
  , name       :: String
  , tokens     :: TransitionF PID Tokens
  }

data Glued a
  = Untouched a
  | Initial a
  | Final a
  | Glued a a

derive instance genericGlued :: Generic (Glued a) _

instance eqGlued :: Eq a => Eq (Glued a) where
  eq = genericEq

instance showGlued :: Show a => Show (Glued a) where
  show = genericShow

isInitial :: ∀ a. Glued a -> Boolean
isInitial = case _ of
  Initial a -> true
  _         -> false

isFinal :: ∀ a. Glued a -> Boolean
isFinal = case _ of
  Final a -> true
  _       -> false

gluedTokens :: Glued Transition -> TransitionF PID Tokens
gluedTokens = case _ of
  Untouched transition              -> transition.tokens
  Initial   transition              -> transition.tokens
  Final     transition              -> transition.tokens
  Glued     transition1 transition2 -> { pre:  transition1.tokens.pre
                                       , post: transition2.tokens.post
                                       }
