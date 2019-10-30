module Language.Statebox.Generator where

import Prelude
import Control.MonadZero (empty)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Either (note)
import Data.Either.Nested (type (\/))
import Data.List as List
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust)
import Data.Foldable (class Foldable, intercalate, foldMap, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map(..))
import Data.Ord (Ordering, compare)
import Data.Tuple (Tuple(..), fst, swap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (for)
import Text.Parsing.Parser.Pos (initialPos) -- TODO don't depend directly on the parsing lib

import Data.Petrinet.Representation.NLL (TransitionF') as NLL
import Language.Statebox.AST (ProtoLabel, Label, LabelAndType, Node(..), MultiEdgeF(..), MultiEdge(..), GElem1F(..), GElem1(..), Span, getProtoLabel, nodeLabel, nodeProtoLabel)
import Statebox.Core.Types (PID, TID, Net)

--------------------------------------------------------------------------------

type TextWithSpan = { text :: String, span :: Span }

-- TODO JS API function, beware of breakage
mkTransitionSymbols :: ParseResult -> Array TextWithSpan
mkTransitionSymbols pr =
  (\(text /\ span) -> { text, span }) <$> pr.syms.transitionLabelsWithSpans


--------------------------------------------------------------------------------
-- Phase 2: construct symbol table based on AST.
--------------------------------------------------------------------------------

type SymbolTable f =
  { name                 :: Maybe TextWithSpan
  , description          :: Maybe TextWithSpan
  , placeLabels          :: f String
  , transitionLabels     :: f String
  | SymbolTableRow' f
  }

type SymbolTable' f = {| SymbolTableRow' f }

type SymbolTableRow' f =
  ( placeLabelsWithSpans      :: f Label
  , transitionLabelsWithSpans :: f Label
  )

mkSymbolTable :: List GElem1 -> SymbolTable Array
mkSymbolTable g =
  { name:                      Nothing -- TODO
  , description:               Nothing -- TODO
  , placeLabels:               Array.fromFoldable $ List.nub symsUnduped.placeLabels
  , transitionLabels:          Array.fromFoldable $ List.nub symsUnduped.transitionLabels
  , placeLabelsWithSpans:      Array.fromFoldable $ List.nub symsUnduped.placeLabelsWithSpans
  , transitionLabelsWithSpans: Array.fromFoldable $ List.nub symsUnduped.transitionLabelsWithSpans
  }
  where
    symsUnduped = { placeLabels:               symsUnduped'.placeLabelsWithSpans      <#> getProtoLabel
                  , transitionLabels:          symsUnduped'.transitionLabelsWithSpans <#> getProtoLabel
                  , placeLabelsWithSpans:      symsUnduped'.placeLabelsWithSpans
                  , transitionLabelsWithSpans: symsUnduped'.transitionLabelsWithSpans
                  }

    symsUnduped' = foldl updateSyms
                         { placeLabelsWithSpans: mempty
                         , transitionLabelsWithSpans: mempty
                         }
                         g

    updateSyms :: ∀ f. Foldable f => Applicative f => Semigroup (f (Node)) => Semigroup (f Label) => Semigroup (f ProtoLabel)
               => SymbolTable' f -> GElem1F f LabelAndType -> SymbolTable' f
    updateSyms { placeLabelsWithSpans, transitionLabelsWithSpans } gelem = case gelem of
      GNode1 n ->
        { placeLabelsWithSpans:      pure (nodeLabel n) <> placeLabelsWithSpans
        , transitionLabelsWithSpans
        }

      GMultiEdge1 (MultiEdge lMaybe srcNodes targetNodes) ->
        { placeLabelsWithSpans:      (nodeLabel <$> srcNodes <> targetNodes) <> placeLabelsWithSpans
        , transitionLabelsWithSpans: transitionLabelsWithSpans <> pure transitionNameAndSpan
        }
        where
          -- TODO in the case of petri (completely specified) petrinets, node labels are mandatory,
          --      so the Maybe should disappear from the edge label in the AST types.
          transitionNameAndSpan = maybe ("TODO unnamed and unnumbered transition" /\ defaultSpan) (\((l /\ span) /\ ty) -> l /\ span) lMaybe
          transitionName        = getProtoLabel transitionNameAndSpan
          defaultSpan = { start: initialPos, end: initialPos } -- TODO nonsensical, remove


--------------------------------------------------------------------------------
-- Phase 3: Based on AST and symbol table, compute some Petri net metadata.
--------------------------------------------------------------------------------

type ParseResult =
  { syms                 :: SymbolTable Array
  , numPlaces            :: Int
  , firstPlaceIndex      :: PID
  , firstTransitionIndex :: TID
  , placeIdsDict         :: Map ProtoLabel PID
  , indexedPlaceLabels   :: Array (PID /\ ProtoLabel)
  }

mkParseResult :: List GElem1 -> ParseResult
mkParseResult g =
  { syms
  , firstPlaceIndex
  , numPlaces
  , firstTransitionIndex
  , indexedPlaceLabels
  , placeIdsDict
  }
  where
    syms = mkSymbolTable g

    firstPlaceIndex      = 1
    numPlaces            = Array.length syms.placeLabels
    firstTransitionIndex = firstPlaceIndex + numPlaces

    indexedPlaceLabels :: Array (PID /\ ProtoLabel)
    indexedPlaceLabels = indexifyFrom firstPlaceIndex syms.placeLabels

    placeIdsDict :: Map ProtoLabel PID
    placeIdsDict = Map.fromFoldable $ map swap $ indexedPlaceLabels

    indexifyFrom i0 = mapWithIndex \i x -> (i+i0) /\ x

-- | Return a transition, or `Nothing` in case the `pid` lookup fails.
multiEdgeToTransition :: ∀ pid. Map ProtoLabel pid -> MultiEdge -> Maybe (NLL.TransitionF' pid)
multiEdgeToTransition pidsDict (MultiEdge lMaybe srcNodes targetNodes) =
  Tuple <$> for (Array.fromFoldable srcNodes)    getPid
        <*> for (Array.fromFoldable targetNodes) getPid
  where
    getPid :: Node -> Maybe pid
    getPid node = Map.lookup (nodeProtoLabel node) pidsDict
