module Language.Statebox.Hypergraph where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bitraversable (class Bitraversable, bitraverse, bisequenceDefault)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap)
import Data.Traversable (class Traversable, traverse)

newtype NodeF v = Node v

unNode :: ∀ v. NodeF v -> v
unNode (Node x) = x

derive instance eqNodeF :: Eq v => Eq (NodeF v)

instance showNodeF :: Show v => Show (NodeF v) where
  show (Node x) = "(NodeF " <> show x <> ")"

derive instance functorNodeF :: Functor NodeF

--------------------------------------------------------------------------------

-- | A hyper edge labeled with type e, going from v's to v's.
data HyperEdgeF f v e = HyperEdge e (f v) (f v)

instance showHyperEdgeF :: (Show v, Show e, Show (f v)) => Show (HyperEdgeF f v e) where
  show (HyperEdge l s t) = "(HyperEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqHyperEdgeF :: (Eq1 f, Eq v, Eq e) => Eq (HyperEdgeF f v e)

instance bifunctorHyperEdgeF :: Functor f => Bifunctor (HyperEdgeF f) where
  bimap f g (HyperEdge e srcs targs) = HyperEdge (g e) (map f srcs) (map f targs)

instance bifoldableHyperEdgeF :: Foldable f => Bifoldable (HyperEdgeF f) where
  bifoldMap f g (HyperEdge e srcs targs) = g e <> foldMap f srcs <> foldMap f targs
  bifoldl = bifoldlDefault
  bifoldr = bifoldrDefault

instance bitraversableHyperEdgeF :: Traversable f => Bitraversable (HyperEdgeF f) where
  bitraverse f g (HyperEdge e srcs targs) = HyperEdge <$> g e <*> traverse f srcs <*> traverse f targs
  bisequence = bisequenceDefault
  
--------------------------------------------------------------------------------

-- | Graph element type.
data GElemF f v e
  = GNode      (NodeF v)
  | GHyperEdge (HyperEdgeF f v e)

derive instance eqGElemF :: (Eq v, Eq (HyperEdgeF f v e)) => Eq (GElemF f v e)

instance bifunctorGElemF :: Functor f => Bifunctor (GElemF f) where
  bimap f g (GNode n)      = GNode (map f n)
  bimap f g (GHyperEdge e) = GHyperEdge (bimap f g e)

instance bifoldableGElemF :: Foldable f => Bifoldable (GElemF f) where
  bifoldMap f g (GNode (Node n)) = f n
  bifoldMap f g (GHyperEdge e)   = bifoldMap f g e
  bifoldl = bifoldlDefault
  bifoldr = bifoldrDefault
  
instance bitraversableGElemF :: Traversable f => Bitraversable (GElemF f) where
  bitraverse f g (GNode (Node n)) = GNode <<< Node <$> f n
  bitraverse f g (GHyperEdge e)   = GHyperEdge <$> bitraverse f g e
  bisequence = bisequenceDefault

instance showGElem :: (Show v, Show e, Show (HyperEdgeF f v e)) => Show (GElemF f v e) where
  show (GNode x)      = "(GNode "      <> show x <> ")"
  show (GHyperEdge x) = "(GHyperEdge " <> show x <> ")"
