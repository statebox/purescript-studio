module Language.Statebox.Net.Generator.Net where

import Prelude
import Control.MonadZero (empty, guard)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Either (note)
import Data.Either.Nested (type (\/))
import Data.List as List
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust)
import Data.Foldable (class Foldable, elem, intercalate, foldMap, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map(..))
import Data.Ord (Ordering, compare)
import Data.Tuple (Tuple(..), fst, swap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (for)
import Language.Statebox.Hypergraph (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Net.AST (Node(..), HyperEdge(..), LabelWithSpan, GElem(..), Label)
import Language.Statebox.Net.Generator

import Data.ArrayMultiset (ArrayMultiset)
import Data.Petrinet.Representation.NLL as NLL
import Data.Petrinet.Representation.NLL (TransitionF') -- TODO factor away
import Statebox.Core.Types (PID, Net)

-- TODO JS API
toNetWithDefaultName :: String -> List GElem -> String \/ Net
toNetWithDefaultName defaultName ast =
  ast # mkParseResult
    >>> toNet ast
    >>> rmap (\(mkNet /\ maybeName) -> mkNet (fromMaybe defaultName maybeName))
    >>> lmap prettyCompileError

toNet :: List GElem -> ParseResult -> CompileError \/ ((String -> Net) /\ Maybe String)
toNet ast pr =
  partitionEither <#> \partition -> mkNet partition /\ nameMaybe
  where
    mkNet partition name = { name:       name
                           , partition:  partition
                           , names:      pr.syms.transitionLabels
                           , placeNames: pr.syms.placeLabels # Just
                           }

    nameMaybe            = pr.syms.name <#> _.text

    partitionEither      = toNLLEither 0 =<< netFEither

    netFEither :: CompileError \/ NetF PID
    netFEither = note PIDLookupFailed $ map Array.fromFoldable $ for ast (mkTransition pr.placeIdsDict)

    mkTransition :: Map Label PID -> GElem -> Maybe (NLL.TransitionF' PID)
    mkTransition placeIdsDict = case _ of
      GNode n      -> empty
      GHyperEdge e -> hyperEdgeToTransition placeIdsDict e

--------------------------------------------------------------------------------

data CompileError
  = SeparatorClash
  | PIDLookupFailed

instance showCompileError :: Show CompileError where
  show = case _ of
    PIDLookupFailed  -> "PIDLookupFailed"
    SeparatorClash   -> "SeparatorClash"

prettyCompileError :: CompileError -> String
prettyCompileError = case _ of
  PIDLookupFailed -> "Failed to look up PID."
  SeparatorClash  -> "Identifier clashes with separator in NLL encoding."

--------------------------------------------------------------------------------

-- TODO get from Data.Petrinet.Representation.NLL (module currently resides in 'Studio')
type NetF p = Array (TransitionF' p)

-- - TODO move to Data.Petrinet.Representation.NLL (module currently resides in 'Studio')
--
-- - TODO this could conceivably produce illegal results if the a's clash with
--        the a that is the separator, so we should return Either/Maybe.
--
-- - TODO this does not cover the case of clashes under conversion to String, which is what NLL is for
toNLLEither :: ∀ a. Eq a => a -> NetF a -> CompileError \/ Array a
toNLLEither separator net =
  note PIDLookupFailed $ for net encodeTransition <#> intercalate [separator]
  where
    encodeTransition :: ArrayMultiset a /\ ArrayMultiset a -> Maybe (Array a)
    encodeTransition (pre /\ post) = do
      let clashes = separator `elem` pre || separator `elem` post
      guard $ not clashes
      pure $ pre <> [separator] <> post
