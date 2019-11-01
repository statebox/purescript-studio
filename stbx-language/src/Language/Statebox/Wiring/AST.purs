module Language.Statebox.Wiring.AST where

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

stripSpan :: LabelWithSpan -> Label
stripSpan (lws /\ _) = lws

--------------------------------------------------------------------------------

type Node = NodeF LabelWithSpan

type HyperEdge = HyperEdgeF List LabelWithSpan Unit

type GElem = GElemF List LabelWithSpan LabelWithSpan
