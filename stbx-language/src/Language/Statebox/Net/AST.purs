module Language.Statebox.Net.AST where

import Prelude
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldMap)

import Language.Statebox.Hypergraph
import Text.Parsing.Parser.Pos (Position(..))

--------------------------------------------------------------------------------

type Label = String

type Span = { start :: Position, end :: Position }

type LabelWithSpan = Label /\ Span

type Type = String

type LabelWithSpanWithType = LabelWithSpan /\ Maybe Type

stripType :: LabelWithSpanWithType -> LabelWithSpan
stripType (l /\ _) = l

stripSpan :: LabelWithSpan -> Label
stripSpan (lws /\ _) = lws

stripTypeAndSpan :: LabelWithSpanWithType -> Label
stripTypeAndSpan = stripType >>> stripSpan

--------------------------------------------------------------------------------

type Node = NodeF LabelWithSpanWithType

type HyperEdge = HyperEdgeF List LabelWithSpanWithType LabelWithSpanWithType

type GElem = GElemF List LabelWithSpanWithType LabelWithSpanWithType

