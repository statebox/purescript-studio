module View.Diagram.FromNLL (fromNLL, ErrDiagramEncoding(..)) where

import Data.Array hiding (head, tail, length)
import Data.Array.NonEmpty (toNonEmpty)
import Data.Either
import Data.Foldable (length)
import Data.Int (rem)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Vec3 (vec3)
import Prelude
import View.Diagram.Model


-- Data types ---------------------------------------------------------------------------------------------------------

-- | This represents a directed graph using only its edges.
-- | Vertices are stored as the source and targets of every edge (or arrow, since it is directed).
-- | Isolated nodes are represented using identity arrows.
-- | This allows representing cyclic graphs and multiple disconnected graphs.
type GraphArrow a = 
  { source :: a
  , target :: a
  }

data ErrDiagramEncoding
  = ErrArrayNotRectangular
  | ErrGraphIsCyclic

derive instance eqErrorEncodingDiagram :: Eq ErrDiagramEncoding

instance showErrDiagramEncoding :: Show ErrDiagramEncoding where
  show ErrArrayNotRectangular = "Error: Brick Diagram is not perfectly square"
  show ErrGraphIsCyclic = "Error: Brick Diagram describes a cyclic graph"

type DiagramM a = Either ErrDiagramEncoding a

-- | See https://adoring-curie-7b92fd.netlify.com/.
type BrickDiagram a = 
  { width :: Int
  , elements :: Array a
  }

-- | Makes a brick diagram without checking bounds and sizes.
mkBrickDiagramUnsafe :: ∀ a. Int -> Array a -> BrickDiagram a
mkBrickDiagramUnsafe width ops = { width: width, elements: ops }

-- | Safe constructor for brick diagrams, checks that the encoded array has the right size.
mkBrickDiagram :: ∀ a. Int -> Array a -> DiagramM (BrickDiagram a)
mkBrickDiagram w ops | length ops `mod` w == 0 = Right $ mkBrickDiagramUnsafe w ops
                     | otherwise               = Left ErrArrayNotRectangular

-- Brick diagram operations -------------------------------------------------------------------------------------------

-- | Get the height of a brick diagram, assuming it's perfectly rectangular.
height :: ∀ a. BrickDiagram a -> Int
height b = (length b.elements) / b.width

-- | Return the element below the given coordinates, if any.
below :: ∀ a. BrickDiagram a -> Int -> Int -> Maybe a
below b x y = let l = min (height b) (y + 1) in
                  index b.elements (l * b.width + x)

-- Converting brick diagrams to directed graphs -----------------------------------------------------------------------

-- | Parses a `BrickDiagram` into a directed graph without identity arrows.
-- | This ignores all the nodes which are equal to the first argument.
-- | This also checks that the graph is acyclic.
parseBrickToGraph :: ∀ a. Eq a => a -> BrickDiagram a -> DiagramM (Array (GraphArrow a))
parseBrickToGraph empty = checkAcyclic <<< filter (not isSelfArrow) <<< filterNode empty <<< brickToGraph

-- | Same as `parseBrickToGraph` except the `mempty` nodes are ignored
parseBrickToGraphMonoid :: ∀ a. Eq a => Monoid a => BrickDiagram a -> DiagramM (Array (GraphArrow a))
parseBrickToGraphMonoid = parseBrickToGraph mempty

-- | Given a node a and a directed graph, remove all arrows which have a as source or dest
filterNode :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array (GraphArrow a)
filterNode value = filter (not nodeContains value)
  where 
    nodeContains :: a -> GraphArrow a -> Boolean
    nodeContains v a = a.source == v || a.target == v

-- | Checks if an arrow in the graph has the same source and destination.
isSelfArrow :: ∀ a. Eq a => GraphArrow a -> Boolean
isSelfArrow {source, target} = source == target

-- | Map a well formed Brick Diagram to a directed graph.
brickToGraph :: ∀ a. Eq a => BrickDiagram a -> Array (GraphArrow a)
brickToGraph b = mapMaybe (edge b) indices
  where 
    indices = range 0 (length b.elements - 1)

-- | Given a brick diagram and an index, return one of the arrows leaving the node at this index.
edge :: ∀ a. Eq a => BrickDiagram a -> Int -> Maybe (GraphArrow a)
edge b i = { source: _, target: _ } <$> index b.elements i <*> below b xPos yPos
  where 
    yPos = i / b.width
    xPos = i `rem` b.width

-- Converting Directed Graphs to Diagram ------------------------------------------------------------------------------

checkAcyclic :: ∀ a. Eq a => Array (GraphArrow a) -> DiagramM (Array (GraphArrow a))
checkAcyclic graph = if isAcyclic graph then Right graph else Left ErrGraphIsCyclic

isAcyclic :: ∀ a. Eq a => Array (GraphArrow a) -> Boolean
isAcyclic graph = case uncons graph of
  Just {head, tail} -> isAcyclicHelper [] head.source graph
  Nothing -> true

isAcyclicHelper :: ∀ a. Eq a => Array a -> a -> Array (GraphArrow a) -> Boolean
isAcyclicHelper seen current graph = let targets = getTargets current graph in
  all (\t -> (t `not elem` seen) && isAcyclicHelper (t : seen) t graph) targets

-- | Given a node in a graph and the arrows in the graph, return the arrows which have
-- | the node as source.
getSources :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array (GraphArrow a)
getSources node = filter (\a -> a.source == node)

-- | Given a node and the arrows in the graph, return all the nodes which are direct 
-- | targets of the node.
getTargets :: ∀ a. Eq a => a -> Array (GraphArrow a) -> Array a
getTargets node = map _.target <<< getSources node 
                                        
-- Converting naively from BrickDiagram to DiagramInfo ----------------------------------------------------------------

brickToDiagram :: ∀ a. Eq a => Show a => BrickDiagram a -> String -> DiagramInfo
brickToDiagram brick name = { name: name, ops: graphToOps brick }

type ConsecutiveValues a = { value :: a, length :: Int }

-- | This function assumes the brick diagram in argument encodes the information to lay out the diagram, e.g.:
-- | ```
-- | 2
-- | 23
-- | 11
-- | 40
-- | ```
-- | will display the diagram:
-- | ```
-- | [--2--] [--3--]
-- | [------1------]
-- | [--4--]
-- | ```
graphToOps :: ∀ a. Eq a => Show a => BrickDiagram a -> Array Operator
graphToOps { width: width, elements: brick } = 
  let lines = splitLines width brick
      l = mapWithIndex mapOperators $ map packConsecutive lines in
      concat l
  where
    splitLines :: Int -> Array a -> Array (Array a)
    splitLines w array | length array <= w = [array]
                       | otherwise         = take width array : splitLines width (drop width array)

    packConsecutive :: Array a -> Array (ConsecutiveValues a)
    packConsecutive = map ((\n -> { value: NE.head n, length: length n }) <<< toNonEmpty) <<< groupBy (==)

    mapOperators :: Int -> Array (ConsecutiveValues a) -> Array Operator
    mapOperators row line = mapWithIndex (mkOperator row) line

    mkOperator :: Int -> Int -> ConsecutiveValues a -> Operator 
    mkOperator row col value =
      { identifier: show row <> ":" <> show col
      , pos: vec3 row col value.length
      , label: show value.value
      }

-- Converting from NLL ------------------------------------------------------------------------------------------------

nllToBrickDiagram :: Array Int -> DiagramM (BrickDiagram Int)
nllToBrickDiagram nll = case uncons nll of
  Just { head, tail } -> mkBrickDiagram head tail
  Nothing -> Right $ mkBrickDiagramUnsafe 0 []
  
fromNLL :: Array Int -> String -> DiagramM DiagramInfo
fromNLL nll name = do bricks <- nllToBrickDiagram nll
                      _ <- parseBrickToGraph 0 bricks
                      Right $ brickToDiagram bricks name
