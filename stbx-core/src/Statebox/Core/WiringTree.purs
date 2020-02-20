module Statebox.Core.WiringTree where

import Prelude
import Data.Array ((:), head, length, partition, range, tail)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))

import Data.ArrayMultiset (ArrayMultiset)
import Data.Petrinet.Representation.NLL (ErrNetEncoding, TransitionF', fromNLL)
import Statebox.Core.Marking (buildTransitionMarking)
import Statebox.Core.Transition (Glued(..), Transition, isInitial, isFinal)
import Statebox.Core.Types (Diagram, Net, PID, TID, Wiring)

data WiringTree
  = Net Net
  | Diagram Diagram (Array WiringTree)

-- | This function translates a `Wiring` into a `WiringTree`.
-- | For the moment, we forget about diagrams and gluings and we consider only simple nets.
fromWiring :: Wiring -> Maybe WiringTree
fromWiring wiring = Net <$> head wiring.nets

data LinearizationError
  = DiagramNotYetSupported
  | NLLDecodingFailed ErrNetEncoding
  | LenghtMismatchBetweenTransitionsAndNames

derive instance genericLinearizationError :: Generic LinearizationError _

instance eqLinearizationError :: Eq LinearizationError where
  eq = genericEq

instance showLinerizationError :: Show LinearizationError where
  show = genericShow

linearize :: WiringTree -> LinearizationError \/ Array (Glued Transition)
linearize (Net net)                  = linearizeNet net
linearize (Diagram diagram branches) = Left DiagramNotYetSupported

linearizeNet :: Net -> LinearizationError \/ Array (Glued Transition)
linearizeNet net = linearizePartitionsAndNames net.partition net.names

linearizePartitionsAndNames :: ArrayMultiset PID -> Array String -> LinearizationError \/ Array (Glued Transition)
linearizePartitionsAndNames partition names =
  either (NLLDecodingFailed >>> Left) (flip linearizeTransitionsAndNames names) $ fromNLL 0 partition

-- | this differs from the standard implementation of zipWith by the fact that it fails if the inputs are of different
-- | length
zip3With :: forall a b c d. (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Maybe (Array d)
zip3With f as bs cs = case head as, head bs, head cs of
  Nothing , Nothing , Nothing  -> Just []
  (Just x), (Just y), (Just z) -> (f x y z : _) <$> zip3With f (tailSafe as) (tailSafe bs) (tailSafe cs)
    where
      tailSafe :: forall e. Array e -> Array e
      tailSafe = (fromMaybe []) <<< tail
  _       , _       , _        -> Nothing

linearizeTransitionsAndNames :: Array (TransitionF' PID) -> Array String -> LinearizationError \/ Array (Glued Transition)
linearizeTransitionsAndNames transitions names =
  maybe
    (Left LenghtMismatchBetweenTransitionsAndNames)
    (Right <<< sortInitialInteriorFinal)
    (zip3With buildGluedTransition (range 0 (length transitions - 1)) transitions names)

buildGluedTransition :: TID -> TransitionF' PID -> String -> Glued Transition
buildGluedTransition tid (pre /\ post) name =
  case pre, post of
    [], _  -> Initial   gluedTransition
    _ , [] -> Final     gluedTransition
    _ , _  -> Untouched gluedTransition
  where
    gluedTransition = { name, path, transition: tid, tokens: buildTransitionMarking pre post }
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
