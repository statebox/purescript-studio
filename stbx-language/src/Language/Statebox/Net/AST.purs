module Language.Statebox.Net.AST where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Eq (class Eq1)
import Data.List (List(..))
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

import Language.Statebox.Hypergraph
import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)
import Text.Parsing.Parser.Pos (Position(..))

--------------------------------------------------------------------------------

type Label = String
type Span = { start :: Position, end :: Position }
type LabelWithSpan = Label /\ Span
type Type = String
type LabelWithSpanWithType = LabelWithSpan /\ Maybe Type
type Node = NodeF LabelWithSpanWithType
type GElem = GElemF List LabelWithSpanWithType LabelWithSpanWithType
type HyperEdge = HyperEdgeF List LabelWithSpanWithType LabelWithSpanWithType

getLabel :: LabelWithSpan -> Label
getLabel (lws /\ _) = lws

nodeLabelWithSpan :: Node -> LabelWithSpan
nodeLabelWithSpan (Node (l /\ _)) = l

nodeLabel :: Node -> Label
nodeLabel = getLabel <<< nodeLabelWithSpan
