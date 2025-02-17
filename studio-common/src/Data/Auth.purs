module Data.Auth where

import Prelude
import Data.Foldable (class Foldable, elem)
import Data.Newtype (class Newtype, un)
import Data.Set as Set
import Data.Set (Set)

newtype Role = Role Int

derive instance eqRole  :: Eq  Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show (Role x) = show x

--------------------------------------------------------------------------------

-- newtype so we can assign special semantics, eg should always 'contain' admin even though it's not in the set
newtype Roles = Roles (Set Role)

derive instance newtypeRoles :: Newtype Roles _

instance semigroupRoles :: Semigroup Roles where
  append (Roles x) (Roles y) = Roles (append x y)

instance monoidRoles :: Monoid Roles where
 mempty = Roles mempty

instance showRoles :: Show Roles where
  show (Roles x) = show x

rolesFromFoldable :: forall t. Foldable t => t Role -> Roles
rolesFromFoldable = Roles <<< Set.fromFoldable

rolesElem :: Role -> Roles -> Boolean
rolesElem role (Roles roles) = elem role roles

--------------------------------------------------------------------------------

type RoleInfo =
  { id        :: Role
  , name      :: String
  , bgColor   :: CSSColor
  , textColor :: CSSColor
  }

newtype CSSColor = CSSColor String

derive instance newtypeCSSColor :: Newtype CSSColor _

instance showCSSColor :: Show CSSColor where
  show x = "(CSSColor " <> (show (un CSSColor x)) <> ")"

--------------------------------------------------------------------------------

-- TODO define iso Boolean
data Privilege
  = Privileged
  | Unprivileged

derive instance eqPrivilege  :: Eq  Privilege
derive instance ordPrivilege :: Ord Privilege

isPrivileged :: Privilege -> Boolean
isPrivileged = case _ of
  Privileged   -> true
  Unprivileged -> false

toPrivilege :: Boolean -> Privilege
toPrivilege = case _ of
  true  -> Privileged
  false -> Unprivileged
