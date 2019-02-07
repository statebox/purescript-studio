module View.Diagram.FromNLL (fromNLL, ErrDiagramEncoding(..)) where

import Data.Either
import Data.Monoid
import Prelude
import View.Diagram.Model

import Data.Array (length, mapMaybe, range, index, filter, findIndex, uncons, all, (:))
import Data.Int (rem)
import Data.Maybe (Maybe(..))

-- Data types ----------------------------------------------------------------------------

-- This graph representation assumes that there are no isolated nodes
type GraphArrow a = 
  { source :: a
  , target :: a
  }

-- TODO: fix this derived instance
-- derive instance eqGraphArrow :: Eq a => Eq (GraphArrow a)

-- A brick Diagram is caracterized by its width and the array of data 
type BrickDiagram a = 
  { width :: Int
  , elements :: Array a
  }

mkBrickDiagramUnsafe :: ∀ a. Int -> Array a -> BrickDiagram a
mkBrickDiagramUnsafe width ops = { width: width, elements: ops }

-- | Safe constructor for Brick Diagrams
makeDiagram :: ∀ a. Int -> Array a -> Either ErrDiagramEncoding (BrickDiagram a)
makeDiagram width ops | length ops `mod` width == 0 = Right $ mkBrickDiagramUnsafe width ops
                      | otherwise                   = Left ErrArrayNotRectangular

-- Brick Diagram operations ---------------------------------------------------------------

-- This assumes that the Brick diagram is perfectly rectangular
height :: ∀ a. BrickDiagram a -> Int
height b = (length b.elements) / b.width

below :: ∀ a. BrickDiagram a -> Int -> Int -> Maybe a
below b x y = let l = min (height b) (y + 1) in
                  index b.elements (l * b.width + x)

-- Converting Brick Diagrams to Directed graphs -------------------------------------------

data ErrDiagramEncoding = ErrArrayNotRectangular
                        | ErrGraphIsCyclic

derive instance eqErrorEncodingDiagram :: Eq ErrDiagramEncoding

instance showErrDiagramEncoding :: Show ErrDiagramEncoding where
  show ErrArrayNotRectangular = "Error: Brick Diagram is not perfectly square"
  show ErrGraphIsCyclic = "Error: Brick Diagram describes a cyclic graph"

fromNLL :: ∀ a. Eq a => Int -> Array a -> a -> Either ErrDiagramEncoding (Array (GraphArrow a))
fromNLL width ops empty = makeDiagram width ops
  >>= checkAcyclic <<< filterSelfArrow <<< filterNode empty <<< brickToGraph

fromNLLMonoid :: ∀ a. Eq a => Monoid a => Int -> Array a -> Either ErrDiagramEncoding (Array (GraphArrow a))
fromNLLMonoid width ops = fromNLL width ops mempty

-- | Given a node a and a directed graph, remove all arrows which have a as source or dest
filterNode :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array (GraphArrow a)
filterNode value = filter (not <<< nodeContains value)
  where nodeContains :: a -> GraphArrow a -> Boolean
        nodeContains v a = a.source == v || a.target == v

-- | Remove arrows that have the same source and destination
filterSelfArrow :: ∀ a. Eq a => Array (GraphArrow a) -> Array (GraphArrow a)
filterSelfArrow = filter \arr -> arr.source /= arr.target

-- | Map a well formed Brick Diagram to a directed graph
brickToGraph :: ∀ a. Eq a => BrickDiagram a -> Array (GraphArrow a)
brickToGraph b = mapMaybe (edge b) indices
  where indices = range 0 (length b.elements - 1)

-- | Given a Brick diagram and an index, return the arrows between nodes in the graph, if any
edge :: ∀ a. Eq a => BrickDiagram a -> Int -> Maybe (GraphArrow a)
edge b i = do src <- index b.elements i
              trg <- below b xPos yPos
              pure $ { source: src, target: trg }
  where 
        yPos = i / b.width
        xPos = i `rem` b.width

-- Converting Directed Graphs to Diagram ------------------------------------------------

contains :: ∀ a. Eq a =>  Array a -> a -> Boolean
contains array v = findIndex (eq v) array /= Nothing

checkAcyclic :: ∀ a. Eq a => Array (GraphArrow a) -> Either ErrDiagramEncoding (Array (GraphArrow a))
checkAcyclic graph = if isAcyclic graph then Right graph else Left ErrGraphIsCyclic

isAcyclic :: ∀ a. Eq a => Array (GraphArrow a) -> Boolean
isAcyclic graph = case uncons graph of
  Just {head: h, tail: t} -> isAcylicHelper [] h.source graph
  Nothing -> true

isAcylicHelper :: ∀ a. Eq a => Array a -> a -> Array (GraphArrow a) -> Boolean
isAcylicHelper seen current graph = let targets = getTargets current graph in
  all (\t -> not (seen `contains` t) && isAcylicHelper (t : seen) t graph) targets

-- | Given a node in a graph and the arrows in the graph, return the arrows which have the node as source
getSources :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array (GraphArrow a)
getSources node = filter (\a -> a.source == node)

-- | Given a node and the arrows in the graph, return all the nodes which are direct targets of the node
getTargets :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array a
getTargets node = map (\a -> a.target) <<< getSources node 
                                        
graphToDiagram :: ∀ a. Array (GraphArrow a) -> String -> DiagramInfo
graphToDiagram graph name = { name: name, ops: graphToOps graph }

-- What now?
graphToOps :: ∀ a. Array (GraphArrow a) -> Array Operator
graphToOps graph = []
