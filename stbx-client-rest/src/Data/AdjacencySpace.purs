module Data.AdjacencySpace where

import Prelude
import Data.Array (cons)
import Control.Bind ((<=<))
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Plus as Plus
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Map (Map, member)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Nested (type (/\), (/\))

-- | Doubly linked adjacency data structure. Records ids, the (optional) values corresponding to those ids,
-- | and the links between keys and their parent and child keys.
type AdjacencySpace k v =
  { kids      :: Map k (Set k)
  , parents   :: Map k k
  , values    :: Map k v
  }

-- TODO generalise
type Key = String

empty :: ∀ v. AdjacencySpace Key v
empty = { kids: mempty, parents: mempty, values: mempty }

lookup :: ∀ a. Key -> AdjacencySpace Key a -> Maybe a
lookup k s = Map.lookup k s.values

-- TODO optimisation: unify parents and values? maintaining two indices that are the same seems expensive.
update :: ∀ a. (a -> Maybe Key) -> AdjacencySpace Key a -> Key -> a -> AdjacencySpace Key a
update getParentKeyMaybe s curKey val =
  s { kids    = parentMaybe # maybe s.kids    (\parent -> Map.alter  updateKids parent s.kids)
    , parents = parentMaybe # maybe s.parents (\parent -> Map.insert curKey     parent s.parents)
    , values  =                                           Map.insert curKey     val    s.values
    }
  where
    parentMaybe = getParentKeyMaybe val
    updateKids = Just <<< Set.insert curKey <<< fromMaybe mempty

kids :: ∀ f v. Monoid (Set Key) => AdjacencySpace Key v -> Key -> Set Key
kids s k = fromMaybe mempty (Map.lookup k s.kids)

-- | The tree roots of the forest of known keys. The values corresponding to these
-- | keys may be either loaded or unloaded.
rootKeys :: ∀ v. AdjacencySpace Key v -> Set Key
rootKeys s = Set.filter (not isLoaded) parentKeys
  where
    parentKeys = Map.keys s.kids
    isLoaded = isLoadedIn s

parentKey :: ∀ v. AdjacencySpace Key v -> Key -> Maybe Key
parentKey s k = Map.lookup k s.parents

hasParentKey :: ∀ v. AdjacencySpace Key v -> Key -> Boolean
hasParentKey s = isJust <<< parentKey s

isLoadedIn :: ∀ v. AdjacencySpace Key v -> Key -> Boolean
isLoadedIn s k = k `member` s.values

--------------------------------------------------------------------------------

-- | `Left` means that a parent key along the path was not found in the space, and here's the chain up to that point.
-- |
-- | - TODO This does a recursive call which will at some point blow the stack; look at buildCofree.
-- | - TODO This function assumes the given `AdjacencySpace` encodes a tree.
-- |        Computation will diverge in the presence of cycles among the keys and their parents/children.
unsafeAncestorsBetween
  :: ∀ v
   . AdjacencySpace Key v
  -> Key
  -> Key
  -> Either (Array Key) (Array Key)
unsafeAncestorsBetween s from to =
  go [] from
  where
    go :: ∀ v. Array Key -> Key -> Either (Array Key) (Array Key)
    go path k | k == to = Right (k `cons` path)                        -- done
    go path k           = parentKey s k # maybe (Left (k `cons` path)) -- parent key not in space
                                                (go   (k `cons` path)) -- next

-- | - TODO This does a recursive call which will at some point blow the stack; look at buildCofree.
-- | - TODO This function assumes the given `AdjacencySpace` encodes a tree.
-- |        Computation will diverge in the presence of cycles among the keys and their parents/children.
unsafeToTree
  :: ∀ v v'
   . (AdjacencySpace Key v -> Key -> Maybe v -> Array (Cofree Array v') -> Cofree Array v')
  -> AdjacencySpace Key v
  -> Key
  -> Cofree Array v'
unsafeToTree mkNode s k =
  mkNode s k v nodeKids
  where
    v :: Maybe v
    v = lookup k s

    nodeKids :: Array (Cofree Array v')
    nodeKids = unsafeToTree mkNode s <$> Set.toUnfoldable (kids s k)
