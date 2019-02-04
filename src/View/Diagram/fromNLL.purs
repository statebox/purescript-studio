module View.Diagam.FromNLL where

import Data.Monoid
import Prelude
import View.Diagram.Model

import Data.Array (length, mapMaybe, range, index, filter)
import Data.Int (rem)
import Data.Maybe (Maybe(..))

-- This graph representation assumes that there are no isolated nodes
newtype GraphArrow a = GraphArrow 
  { source :: a
  , target :: a
  }
derive instance eqGraphArrow :: Eq a => Eq (GraphArrow a)

-- A brick Diagram is caracterized by its width and the array of data 
type BrickDiagram a = 
  { width :: Int
  , elements :: Array a
  }

-- Brick Diagram operations ---------------------------------------------------------------

-- This assumes that the Brick diagram is perfectly rectangular
height :: forall a. BrickDiagram a -> Int
height b = (length b.elements) / b.width

below :: forall a. BrickDiagram a -> Int -> Int -> Maybe a
below b x y = let l = min (height b) (y + 1) in
                  index b.elements (l * b.width + x)


-- Converting Brick Diagrams to Directed graphs -------------------------------------------

brickToGraphMonoid :: forall a. Eq a => Monoid a => BrickDiagram a -> Array (GraphArrow a)
brickToGraphMonoid = filterNode mempty <<< brickToGraph 

filterNode :: forall a. Eq a => a -> Array (GraphArrow a) -> Array (GraphArrow a)
filterNode value = filter (nodeContains value)
  where nodeContains :: a -> GraphArrow a -> Boolean
        nodeContains v (GraphArrow r) = r.source == v || r.target == v

brickToGraph :: forall a. Eq a => BrickDiagram a -> Array (GraphArrow a)
brickToGraph b = mapMaybe (edge b) indices
  where indices = range 0 (length b.elements - 1)

-- Given a Brick diagram and an index, return the arrows between nodes in the graph, if any
edge :: forall a. Eq a => BrickDiagram a -> Int -> Maybe (GraphArrow a)
edge b i = do src <- index b.elements i
              trg <- below b xPos yPos
              pure $ GraphArrow { source : src, target : trg }
  where 
        yPos = i / b.width
        xPos = i `rem` b.width

-- Converting Directed Graphs to Diagram ------------------------------------------------
graphToDiagram :: forall a. Array (GraphArrow a) -> String -> DiagramInfo
graphToDiagram graph name = { name : name, ops : graphToOps graph }

-- What now?
graphToOps :: forall a. Array (GraphArrow a) -> Array Operator
graphToOps graph = []
