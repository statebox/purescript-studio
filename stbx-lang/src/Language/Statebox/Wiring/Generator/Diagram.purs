module Language.Statebox.Wiring.Generator.Diagram where

import Prelude
import Data.Array (length, elemIndex)
import Data.Maybe (maybe)
import Data.Foldable (foldMap)
import Data.Bitraversable (bitraverse)
import Data.Traversable (traverse)
import Data.Traversable.Accum.Internal (StateL(..), stateL)
import Data.List (List)

import Language.Statebox.Hypergraph (HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (GElem, stripSpan)
import Statebox.Core.Types (Diagram)

toDiagramWithName :: String -> List GElem -> Diagram
toDiagramWithName name ast =
  { name, width, pixels, names: acc.accum }
  where
    acc = traverse (bitraverse (stripSpan >>> lookupOrAdd) idStateL) ast # (_ `stateL` [])
    getEdges (GHyperEdge (HyperEdge _ srcs targs)) = foldMap (\src -> foldMap (\targ -> [{ src, targ }]) targs) srcs
    getEdges _                                     = []
    edges = foldMap getEdges acc.value
    width = length edges
    pixels = (edges <#> _.src) <> (edges <#> _.targ)

lookupOrAdd :: ∀ v. Eq v => v -> StateL (Array v) Int
lookupOrAdd v =
  StateL $ \vs -> elemIndex v vs # maybe { accum: vs <> [v], value: length vs + 1 }
                                         \i -> { accum: vs, value: i + 1 }

idStateL :: ∀ a s. a -> StateL s a
idStateL value = StateL $ \accum -> { accum, value }
