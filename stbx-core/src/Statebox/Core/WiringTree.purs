module Statebox.Core.WiringTree where

import Prelude
import Control.Apply (lift3)
import Data.Array (head, length, partition, range)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Data.ArrayMultiset (ArrayMultiset)
import Data.Petrinet.Representation.NLL (ErrNetEncoding, TransitionF', fromNLL)
import Statebox.Core.Execution (Path)
import Statebox.Core.Types (Diagram, Net, PID, TID, Wiring)

data WiringTree
  = Net Net
  | Diagram Diagram (Array WiringTree)

-- | This function translates a `Wiring` into a `WiringTree`
-- | For the moment, we forget about diagrams and gluings and we consider only simple nets
wiringToWiringTree :: Wiring -> Maybe WiringTree
wiringToWiringTree wiring = Net <$> head wiring.nets

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

isInitial :: forall a. Glued a -> Boolean
isInitial = case _ of
  Initial a -> true
  _         -> false

isFinal :: forall a. Glued a -> Boolean
isFinal = case _ of
  Final a -> true
  _       -> false

data LinerisationError
  = DiagramNotYetAllowed
  | NLLDecodingFailed ErrNetEncoding

linearise :: WiringTree -> Either LinerisationError (Array (Glued Transition))
linearise (Net net)                  = lineariseNet net
linearise (Diagram diagram branches) = Left DiagramNotYetAllowed

lineariseNet :: Net -> Either LinerisationError (Array (Glued Transition))
lineariseNet net = linearisePartitionsAndNames net.partition net.names

linearisePartitionsAndNames :: ArrayMultiset PID -> Array String -> Either LinerisationError (Array (Glued Transition))
linearisePartitionsAndNames partition names =
  either (NLLDecodingFailed >>> Left) (Right <<< flip lineriseTransitionsAndNames names) $ fromNLL 0 partition

-- the use of `lift3` does not consider the fact that the arrays could in principle have different lenghts
lineriseTransitionsAndNames :: Array (TransitionF' PID) -> Array String -> Array (Glued Transition)
lineriseTransitionsAndNames transitions names =
  sortInitialFinal $ lift3 buildGluedTransition (range 0 (length transitions - 1)) transitions names

buildGluedTransition :: TID -> TransitionF' PID -> String -> Glued Transition
buildGluedTransition tId (inputs /\ outputs) name =
  case (inputs /\ outputs) of
    ([]  /\ _  ) -> Initial   { name: name, path: [0, 0, 0], transition: tId } -- the path is [0, 0, 0] because we consider a trivial diagram to be there
    (_   /\ [] ) -> Final     { name: name, path: [0, 0, 0], transition: tId }
    (inp /\ out) -> Untouched { name: name, path: [0, 0, 0], transition: tId }

-- | we are using this custom function instead of `sortBy` because that does not guarantee
-- | the order of equal things to be preserved
sortInitialFinal :: forall a. Array (Glued a) -> Array (Glued a)
sortInitialFinal gluedItems =
  let { no: notInitial        , yes: initial } = partition isInitial gluedItems
      { no: notInitialAndFinal, yes: final   } = partition isFinal   notInitial
  in initial <> notInitialAndFinal <> final
