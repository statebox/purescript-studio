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

type ProtoLabel = String
type Span = { start :: Position, end :: Position }
type Label = ProtoLabel /\ Span
type Type = String
type LabelAndType = Label /\ Maybe Type
type Node = NodeF LabelAndType
type GElem1 = GElem1F List LabelAndType
type MultiEdge = MultiEdgeF List LabelAndType

getProtoLabel :: Label -> ProtoLabel
getProtoLabel (p /\ _) = p

nodeLabel :: Node -> Label
nodeLabel (Node (l /\ _)) = l

nodeProtoLabel :: Node -> ProtoLabel
nodeProtoLabel = getProtoLabel <<< nodeLabel

--------------------------------------------------------------------------------

newtype NodeF a = Node a

unNode :: forall f a. NodeF a -> a
unNode (Node x) = x

derive instance eqNodeF :: Eq a => Eq (NodeF a)

instance showNodeF :: Show a => Show (NodeF a) where
  show (Node x) = "(NodeF " <> show x <> ")"

derive instance functorNodeF :: Functor NodeF

--------------------------------------------------------------------------------

-- | A hyperedge labeled with type a, going from a's to a's.
data MultiEdgeF f a = MultiEdge (Maybe a) (f (NodeF a)) (f (NodeF a))

instance showMultiEdgeF :: (Show a, Show (f (NodeF a))) => Show (MultiEdgeF f a) where
  show (MultiEdge l s t) = "(MultiEdge " <> show l <> " " <> show s <> " " <> show t <> ")"

derive instance eqMultiEdgeF :: (Eq1 f, Eq a) => Eq (MultiEdgeF f a)

derive instance functorMultiEdgeF :: Functor f => Functor (MultiEdgeF f)

--------------------------------------------------------------------------------

-- | Generic graph element type.
data GElem1F f a = GNode1      (NodeF a)
                 | GMultiEdge1 (MultiEdgeF f a)

derive instance eqGElem1F :: (Eq a, Eq (MultiEdgeF f a)) => Eq (GElem1F f a)

derive instance functorGElem1F :: Functor f => Functor (GElem1F f)

instance showGElem1 :: (Show a, Show (MultiEdgeF f a)) => Show (GElem1F f a) where
  show (GNode1 x)      = "(GNode1 "      <> show x <> ")"
  show (GMultiEdge1 x) = "(GMultiEdge1 " <> show x <> ")"
