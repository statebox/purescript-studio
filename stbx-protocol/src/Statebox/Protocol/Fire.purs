module Statebox.Protocol.Fire where

import Prelude
import Data.Array (index)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.NonEmpty (head)

import Data.Petrinet.Representation.Marking (MarkingF)
import Data.Petrinet.Representation.Dict (fireEnabledAtMarking)
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

derive instance genericFiringError :: Generic FiringError _

instance eqFiringError :: Eq FiringError where
  eq = genericEq

instance showFiringError :: Show FiringError where
  show = genericShow

fire :: Wiring -> Marking -> Firing -> FiringError \/ Marking
fire wiring marking firing = maybe
  (Left FireInvalidWiringTree)
  (\wiringTree ->
    either
      (Left <<< FireLinearizationError)
      (\gluedTransitions ->
        let transitionIndex = head firing.path
        in maybe
          (Left FireTransitionIndexOutOfBounds)
          (\gluedTransition ->
            maybe
              (Left FireTransitionNotEnabled)
              Right
              (fireEnabledAtMarking marking $ gluedTokens gluedTransition))
          (index gluedTransitions transitionIndex))
      (linearize wiringTree))
  (fromWiring wiring)

fireMultiple :: Wiring -> Marking -> Array Firing -> FiringError \/ Marking
fireMultiple wiring = foldM (fire wiring)

-- TODO dedupe
type Marking = MarkingF PID Tokens
type Tokens = Int
