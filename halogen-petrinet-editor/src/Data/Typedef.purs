module Data.Typedef where

import Data.Newtype (class Newtype)

-- | A placeholder type, used for type definitions that can be typed in in the
-- | form of Strings using the Petri net editor. When we have a more mature
-- | version of something like this, we can move this whole thing out of the
-- | Petri net editor package.
newtype Typedef = Typedef String

derive instance newtypeTypedef :: Newtype Typedef _

type TypeName = String
