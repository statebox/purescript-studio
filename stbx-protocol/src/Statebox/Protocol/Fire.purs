module Statebox.Protocol.Fire where

import Prelude
import Data.Array (index)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Maybe (maybe)
import Data.NonEmpty (head)

import Data.Petrinet.Representation.Dict (fireAtMarking)
import Data.Petrinet.Representation.Marking (MarkingF)
import Statebox.Core.Transition (gluedTokens)
import Statebox.Core.Types (Firing, Wiring, PID)
import Statebox.Core.WiringTree (LinearizationError, fromWiring, linearize)

data FiringError

  -- | The wiring does not describe a valid wiring tree.
  = FireInvalidWiringTree

  -- | The linearization of the wiring tree failed with a LinearizationError
  | FireLinearizationError LinearizationError

  -- | The path of the firing describing
  | FireTransitionIndexOutOfBounds

  -- | The selected transition is not enabled
  | FireTransitionNotEnabled

fire :: Wiring -> Firing -> Marking -> FiringError \/ Marking
fire wiring firing marking = maybe
  (Left FireInvalidWiringTree)
  (\wiringTree ->
    either
      (Left <<< FireLinearizationError)
      (\gluedTransitions ->
        let transitionIndex = head firing.path
        in maybe
          (Left FireTransitionIndexOutOfBounds)
          (\gluedTransition ->
            Right $ fireAtMarking marking $ gluedTokens gluedTransition)
          (index gluedTransitions transitionIndex))
      (linearize wiringTree))
  (fromWiring wiring)

-- TODO dedupe
type Marking = MarkingF PID Tokens
type Tokens = Int
