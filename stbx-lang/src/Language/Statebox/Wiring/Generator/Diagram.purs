module Language.Statebox.Wiring.Generator.Diagram where

import Prelude
import Data.Array (length)
import Data.List (List)

import Language.Statebox.Wiring.Generator (toIndexedGraph, getEdges)
import Language.Statebox.Wiring.AST (GElem)
import Statebox.Core.Types (Diagram)

toDiagramWithName :: String -> List GElem -> Diagram
toDiagramWithName name ast =
  { name, width, pixels, names }
  where
    { graph, names } = toIndexedGraph ast
    edges = getEdges graph
    width = length edges
    pixels = (edges <#> _.src) <> (edges <#> _.tgt)
