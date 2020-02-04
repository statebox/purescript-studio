module Statebox.Protocol.Fire where

import Prelude
import Data.Array (index)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Maybe (maybe)
import Data.NonEmpty (head)

import Data.Petrinet.Representation.Dict (fireAtMarking)
import Statebox.Core.Transition (gluedTokens)
import Statebox.Core.Types (Firing, Wiring)
import Statebox.Core.WiringTree (LinearizationError, fromWiring, linearize)
import View.Petrinet.Model (Marking)

data FiringError

  -- | The wiring does not describe a valid wiring tree.
  = FireInvalidWiringTree

  -- | The linerization of the wiring tree failed with a LinerizationError
  | FireLinerizationError LinearizationError

  -- | The path of the firing describing
  | FireTransitionIndexOutOfBounds

  -- | The selected transition is not enabled
  | FireTransitionNotEnabled

fire :: Wiring -> Firing -> Marking -> FiringError \/ Marking
fire wiring firing marking = maybe
  (Left FireInvalidWiringTree)
  (\wiringTree ->
    either
      (Left <<< FireLinerizationError)
      (\gluedTransitions ->
        let transitionIndex = head firing.path
        in maybe
          (Left FireTransitionIndexOutOfBounds)
          (\gluedTransition ->
            Right $ fireAtMarking marking $ gluedTokens gluedTransition)
          (index gluedTransitions transitionIndex))
      (linearize wiringTree))
  (fromWiring wiring)
