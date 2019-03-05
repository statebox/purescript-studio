module Data.AdjacencySpace where

import Prelude hiding
import Control.Bind ((<=<))
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Plus as Plus
import Control.Plus (class Plus)
import Data.Map as Map
import Data.Map (Map, member)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Set as Set
import Data.Set (Set)

-- | Adjacency data structure. Records ids, the values corresponding to those ids, and the links between ids and parent ids.

type AdjacencySpace k v =
  { kids      :: Map k (Set k)
  , parents   :: Map k k
  , values    :: Map k v
  }

-- TODO generalise
type Key = String

empty :: ∀ v. AdjacencySpace Key v
empty = { kids: mempty, parents: mempty, values: mempty }

-- TODO optimisation: unify parents and values? maintaining two indices that are the same seems expensive.
update :: ∀ a. (a -> Key) -> AdjacencySpace Key a -> Key -> a -> AdjacencySpace Key a
update getParent t curKey val =
  t { kids    = Map.alter          (Just <<< Set.insert curKey <<< fromMaybe mempty) parent t.kids
    , parents = Map.insert curKey parent                                                  t.parents
    , values  = Map.insert curKey val                                                     t.values
    }
  where
    parent = getParent val

kids :: ∀ f v. Monoid (Set Key) => AdjacencySpace Key v -> Key -> Set Key
kids t hash = fromMaybe mempty (Map.lookup hash t.kids)

-- | The tree roots of the forest of known hashes. The values corresponding to these
-- | hashes may be either loaded or unloaded.
rootKeys :: ∀ v. AdjacencySpace Key v -> Set Key
rootKeys t = Set.filter (not isLoaded) parentKeys
  where
    parentKeys = Map.keys t.kids
    isLoaded = isLoadedIn t

parentKey :: ∀ v. AdjacencySpace Key v -> Key -> Maybe Key
parentKey t h = Map.lookup h t.parents

hasParentKey :: ∀ v. AdjacencySpace Key v -> Key -> Boolean
hasParentKey t = isJust <<< parentKey t

isLoadedIn :: ∀ v. AdjacencySpace Key v -> Key -> Boolean
isLoadedIn t h = h `member` t.values

--------------------------------------------------------------------------------

-- | - TODO This does a recursive call which will at some point blow the stack; look at buildCofree.
-- | - TODO This function assumes the given `AdjacencySpace` encodes a tree.
-- |        Computation will diverge in the presence of cycles among the hash codes and their parents.
unsafeToTree
  :: forall v b
   . (AdjacencySpace Key v -> Key -> Maybe v -> Array (Cofree Array b) -> Cofree Array b)
  -> AdjacencySpace Key v
  -> Key
  -> Cofree Array b
unsafeToTree mkNode t hash =
  mkNode t hash (Map.lookup hash t.values) nodeKids
  where
    nodeKids :: Array (Cofree Array b)
    nodeKids = unsafeToTree mkNode t <$> Set.toUnfoldable (kids t hash)
