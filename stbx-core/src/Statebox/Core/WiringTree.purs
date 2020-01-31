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
import Statebox.Core.Execution (Path)
import Statebox.Core.Types (Diagram, Net, PID, TID, Wiring)

data WiringTree
  = Net Net
  | Diagram Diagram (Array WiringTree)

-- | This function translates a `Wiring` into a `WiringTree`.
-- | For the moment, we forget about diagrams and gluings and we consider only simple nets.
fromWiring :: Wiring -> Maybe WiringTree
fromWiring wiring = Net <$> head wiring.nets

type Transition =
  { path       :: Path
  , transition :: TID
  , name       :: String
  }

data Glued a
  = Untouched a
  | Initial a
  | Final a
  | Glued a a

isInitial :: ∀ a. Glued a -> Boolean
isInitial = case _ of
  Initial a -> true
  _         -> false

isFinal :: ∀ a. Glued a -> Boolean
isFinal = case _ of
  Final a -> true
  _       -> false

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

-- the use of `lift3` does not consider the fact that the arrays could in principle have different lengths
linearizeTransitionsAndNames :: Array (TransitionF' PID) -> Array String -> Array (Glued Transition)
linearizeTransitionsAndNames transitions names =
  sortInitialFinal $ lift3 buildGluedTransition (range 0 (length transitions - 1)) transitions names

buildGluedTransition :: TID -> TransitionF' PID -> String -> Glued Transition
buildGluedTransition tId (inputs /\ outputs) name =
  case (inputs /\ outputs) of
    ([]  /\ _  ) -> Initial   { name: name, path: [0, 0, 0], transition: tId } -- the path is [0, 0, 0] because we consider a trivial diagram to be there
    (_   /\ [] ) -> Final     { name: name, path: [0, 0, 0], transition: tId }
    (inp /\ out) -> Untouched { name: name, path: [0, 0, 0], transition: tId }

-- | We use this custom function instead of `sortBy` because that does not guarantee
-- | the order of equal elements to be preserved.
sortInitialFinal :: ∀ a. Array (Glued a) -> Array (Glued a)
sortInitialFinal gluedItems =
  let { no: notInitial        , yes: initial } = partition isInitial gluedItems
      { no: notInitialAndFinal, yes: final   } = partition isFinal   notInitial
  in initial <> notInitialAndFinal <> final
