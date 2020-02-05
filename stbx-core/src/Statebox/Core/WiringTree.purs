module Statebox.Core.WiringTree where

import Prelude
import Control.Apply (lift3)
import Data.Array (head, length, partition, range)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Data.ArrayMultiset (ArrayMultiset)
import Data.Petrinet.Representation.NLL (ErrNetEncoding, TransitionF', fromNLL)
import Statebox.Core.Transition (Glued(..), Transition, buildTokens, isInitial, isFinal)
import Statebox.Core.Types (Diagram, Net, PID, TID, Wiring)

data WiringTree
  = Net Net
  | Diagram Diagram (Array WiringTree)

-- | This function translates a `Wiring` into a `WiringTree`.
-- | For the moment, we forget about diagrams and gluings and we consider only simple nets.
fromWiring :: Wiring -> Maybe WiringTree
fromWiring wiring = Net <$> head wiring.nets

data LinearizationError
  = DiagramNotYetAllowed
  | NLLDecodingFailed ErrNetEncoding

linearize :: WiringTree -> LinearizationError \/ Array (Glued Transition)
linearize (Net net)                  = linearizeNet net
linearize (Diagram diagram branches) = Left DiagramNotYetAllowed

linearizeNet :: Net -> LinearizationError \/ Array (Glued Transition)
linearizeNet net = linearizePartitionsAndNames net.partition net.names

linearizePartitionsAndNames :: ArrayMultiset PID -> Array String -> LinearizationError \/ Array (Glued Transition)
linearizePartitionsAndNames partition names =
  either (NLLDecodingFailed >>> Left) (Right <<< flip linearizeTransitionsAndNames names) $ fromNLL 0 partition

-- TODO `lift3` does not consider the case where the arrays have different lengths; so add check and error
linearizeTransitionsAndNames :: Array (TransitionF' PID) -> Array String -> Array (Glued Transition)
linearizeTransitionsAndNames transitions names =
  sortInitialInteriorFinal $ lift3 buildGluedTransition (range 0 (length transitions - 1)) transitions names

buildGluedTransition :: TID -> TransitionF' PID -> String -> Glued Transition
buildGluedTransition tid (pre /\ post) name =
  case pre, post of
    [], _  -> Initial   gluedTransition
    _ , [] -> Final     gluedTransition
    _ , _  -> Untouched gluedTransition
  where
    gluedTransition = { name, path, transition: tid, tokens: buildTokens pre post }
    path = [netIndex, diagramIndex, 0] -- path to trivial diagram that is assumed to exist
    diagramIndex = 0
    netIndex = 0

-- | We use this custom function instead of `sortBy` because that does not guarantee
-- | the order of equal elements (wrt the initial/interior/final ordering) to be preserved.
sortInitialInteriorFinal :: ∀ a. Array (Glued a) -> Array (Glued a)
sortInitialInteriorFinal gluedItems =
  initial <> interior <> final
  where
    { no: notInitial, yes: initial } = partition isInitial gluedItems
    { no: interior  , yes: final   } = partition isFinal   notInitial
