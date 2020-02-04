module Language.Statebox.Wiring.Generator.DiagramV2 where

import Prelude
import Data.Array (zipWith, take, drop, concat, length, (..), (!!), uncons, elemIndex, filter)
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (class Foldable, maximum, intercalate, foldMap, fold, notElem)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.Map (Map, fromFoldableWith, lookup, union, toUnfoldable)
import Data.Map.Internal (keys)
import Data.Maybe (maybe, fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.TraversableWithIndex (mapAccumLWithIndex)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Function.Memoize (memoize, class Tabulate)
import Statebox.Core.Types (Diagram)

import Language.Statebox.Wiring.Generator (Edges, toIndexedGraph, getEdges)
import Language.Statebox.Wiring.AST (GElem)

-- | A kdmoncat-compatible diagram in source code representation.
type DiagramV2 =
  { pixels  :: String
  , context :: String
  }

fromDiagramAst :: List GElem -> DiagramV2
fromDiagramAst ast = fromEdges (_ - 1) name edges
  where
    { graph, names } = toIndexedGraph ast
    edges = getEdges graph
    name id = names !! (id - 1) # fromMaybe "?"

fromDiagram :: Diagram -> DiagramV2
fromDiagram { width, pixels, names } = fromEdges (_ - 1) name edges
  where
    rows = chunks width pixels
    edges = concat $ zipWith (zipWith (\src tgt -> { src, tgt })) rows (drop 1 rows)
    name id = names !! (id - 1) # fromMaybe "?"

fromEdges :: ∀ a. Ord a => Tabulate a => (a -> Int) -> (a -> String) -> Edges a -> DiagramV2
fromEdges fromEnum name edges = { pixels, context }
  where
    pixels = (0 .. height) <#> row # intercalate "\n"
    context = [nodeTypes, swapTypes] # intercalate "\n"

    predecessors :: Map a (Array a)
    predecessors = edges <#> (\{ src, tgt } -> tgt /\ [src]) # mfromFoldable

    successors :: Map a (Array a)
    successors = edges <#> (\{ src, tgt } -> src /\ [tgt]) # mfromFoldable

    inputs :: Map a (Array Int)
    inputs = edges # mapWithIndex (\i { tgt } -> tgt /\ [i]) # mfromFoldable

    outputs :: Map a (Array Int)
    outputs = edges # mapWithIndex (\i { src } -> src /\ [i]) # mfromFoldable

    level :: a -> Int
    level = memoize \a -> mlookup a predecessors <#> level # maximum # maybe 0 (_ + 1)

    nodes :: List a
    nodes = (successors `union` predecessors) # keys

    grouped :: Array (Array a)
    grouped = nodes <#> (\id -> level id /\ [id]) # mfromFoldable # toUnfoldable <#> snd

    width = length grouped
    height = grouped <#> length # maximum # fromMaybe 0

    typeStr :: a -> Map a (Array a) -> (a -> String) -> String
    typeStr a m f = mlookup a m <#> f # intercalate " "

    pixel :: a -> String
    pixel a = nextChar 'a' (fromEnum a)

    nodeType :: a -> String
    nodeType a = name a <> "@" <> pixel a <> ": " <> typeStr a predecessors (\b -> pixel b <> pixel a)
                                        <> " -> " <> typeStr a successors   (\b -> pixel a <> pixel b)
    nodeTypes :: String
    nodeTypes = map nodeType nodes # intercalate "\n"

    row :: Int -> String
    row y = grouped # foldMapWithIndex \x g ->
      ((g !! y) # maybe (if x > 0 && x < width - 1 then nextChar 'N' (x - 1) else " ") pixel) <>
      if x < width - 1 then nextChar 'A' x else ""

    swapTypes :: String
    swapTypes = grouped
              # uncons
             <#> (\{ head, tail } -> mapAccumLWithIndex mkSwap (levelOutputs head) tail)
              # maybe "" (_.value >>> intercalate "\n")

    mkSwap :: Int -> Array Int -> Array a -> { accum :: Array Int, value :: String }
    mkSwap i edgeIds as = { accum: levelOutputs as <> rest, value }
      where
        value = nextChar 'A' i <> ": [" <> intercalate " " order <> "]\n" <>
                nextChar 'N' i <> ": [" <> intercalate " " ((1 ..< (length rest + 1)) <#> show) <> "]"
        ids = foldMap (\a -> mlookup a inputs) as
        order = (ids <> rest) <#> \id -> elemIndex id edgeIds # maybe "?" ((_ + 1) >>> show)
        rest = filter (\id -> id `notElem` ids) edgeIds

    levelOutputs :: Array a -> Array Int
    levelOutputs = foldMap (\a -> mlookup a outputs)

mfromFoldable :: ∀ f k v. Foldable f => Ord k => Monoid v => f (k /\ v) -> Map k v
mfromFoldable = fromFoldableWith (flip (<>))

mlookup :: ∀ k v. Ord k => Monoid v => k -> Map k v -> v
mlookup k = lookup k >>> fold

nextChar :: Char -> Int -> String
nextChar c i = fromCharCode (toCharCode c + i) # maybe "?" singleton

rangeEx :: Int -> Int -> Array Int
rangeEx x y = if y > x then x .. (y - 1) else []

infix 8 rangeEx as ..<

chunks :: ∀ a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = [take n xs] <> (chunks n $ drop n xs)
