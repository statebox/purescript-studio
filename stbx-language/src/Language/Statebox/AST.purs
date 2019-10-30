module Language.Statebox.AST where

import Prelude
import Data.Eq (class Eq1)
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)
import Text.Parsing.Parser.Pos (Position(..))

--------------------------------------------------------------------------------

type Label = String
type Span = { start :: Position, end :: Position }
type LabelWithSpan = Label /\ Span
type Type = String
type LabelWithSpanWithType = LabelWithSpan /\ Maybe Type
type Node = NodeF LabelWithSpanWithType
type GElem = GElemF List LabelWithSpanWithType
type HyperEdge = HyperEdgeF List LabelWithSpanWithType

getLabel :: LabelWithSpan -> Label
getLabel (p /\ _) = p

nodeLabelWithSpan :: Node -> LabelWithSpan
nodeLabelWithSpan (Node (l /\ _)) = l

nodeLabel :: Node -> Label
nodeLabel = getLabel <<< nodeLabelWithSpan

--------------------------------------------------------------------------------

newtype NodeF a = Node a

unNode :: forall a. NodeF a -> a
unNode (Node x) = x

derive instance eqNodeF :: Eq a => Eq (NodeF a)

instance showNodeF :: Show a => Show (NodeF a) where
  show (Node x) = "(NodeF " <> show x <> ")"

derive instance functorNodeF :: Functor NodeF

--------------------------------------------------------------------------------

-- | A hyperedge labeled with type a, going from a's to a's.
data HyperEdgeF f a = HyperEdge (Maybe a) (f (NodeF a)) (f (NodeF a))

instance showHyperEdgeF :: (Show a, Show (f (NodeF a))) => Show (HyperEdgeF f a) where
  show (HyperEdge l s t) = "(HyperEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqHyperEdgeF :: (Eq1 f, Eq a) => Eq (HyperEdgeF f a)

derive instance functorHyperEdgeF :: Functor f => Functor (HyperEdgeF f)

--------------------------------------------------------------------------------

-- | Generic graph element type.
data GElemF f a = GNode      (NodeF a)
                | GHyperEdge (HyperEdgeF f a)

derive instance eqGElemF :: (Eq a, Eq (HyperEdgeF f a)) => Eq (GElemF f a)

derive instance functorGElemF :: Functor f => Functor (GElemF f)

instance showGElem :: (Show a, Show (HyperEdgeF f a)) => Show (GElemF f a) where
  show (GNode x)      = "(GNode "      <> show x <> ")"
  show (GHyperEdge x) = "(GHyperEdge " <> show x <> ")"
