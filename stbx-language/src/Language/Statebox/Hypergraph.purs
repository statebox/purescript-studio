module Language.Statebox.Hypergraph where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Eq (class Eq1)
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)
import Text.Parsing.Parser.Pos (Position(..))

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

--------------------------------------------------------------------------------

-- | Graph element type.
data GElemF f v e
  = GNode      (NodeF v)
  | GHyperEdge (HyperEdgeF f v e)

derive instance eqGElemF :: (Eq v, Eq (HyperEdgeF f v e)) => Eq (GElemF f v e)

instance bifunctorGElemF :: Functor f => Bifunctor (GElemF f) where
  bimap f g (GNode n)      = GNode (map f n)
  bimap f g (GHyperEdge e) = GHyperEdge (bimap f g e)

instance showGElem :: (Show v, Show e, Show (HyperEdgeF f v e)) => Show (GElemF f v e) where
  show (GNode x)      = "(GNode "      <> show x <> ")"
  show (GHyperEdge x) = "(GHyperEdge " <> show x <> ")"