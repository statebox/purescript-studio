module Language.Statebox.Wiring.Generator where

import Prelude
import Data.Array (length, elemIndex)
import Data.Maybe (maybe)
import Data.Foldable (foldMap)
import Data.Bitraversable (bitraverse)
import Data.List (List)
import Data.Traversable (traverse)
import Data.Traversable.Accum.Internal (StateL(..), stateL)

import Language.Statebox.Hypergraph (HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (GElem, stripSpan)


toIndexedGraph :: List GElem -> { graph :: List (GElemF List Int Unit), names :: Array String }
toIndexedGraph ast = { graph: acc.value, names: acc.accum }
  where
    acc = traverse (bitraverse (stripSpan >>> lookupOrAdd) idStateL) ast # (_ `stateL` [])

lookupOrAdd :: ∀ v. Eq v => v -> StateL (Array v) Int
lookupOrAdd v =
  StateL $ \vs -> elemIndex v vs # maybe { accum: vs <> [v], value: length vs + 1 }
                                          \i -> { accum: vs, value: i + 1 }

idStateL :: ∀ a s. a -> StateL s a
idStateL value = StateL $ \accum -> { accum, value }


type Edges a = Array { src :: a, tgt :: a }

getEdges :: ∀ a. List (GElemF List a Unit) -> Edges a
getEdges = foldMap f
  where
    f (GHyperEdge (HyperEdge _ srcs tgts)) = foldMap (\src -> foldMap (\tgt -> [{ src, tgt }]) tgts) srcs
    f _                                    = []
