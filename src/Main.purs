module Main where

import Prelude
import Control.MonadZero (empty)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import ExampleData as Ex
import PetrinetView as PetrinetView
import Studio as Studio

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody

  -- Two use cases for PetriNetView:
  --
  -- 1) As a standalone viewer; in this case we want a simple way to pass in an initial net.
  -- 1) As an editor component in some larger framework; don't need initial net; use LoadNet instead.

  -- -- Use case 1: Standalone viewer:
  -- runUI (PetrinetView.ui { net: Ex.net1, netApi: Ex.netApi1 }) unit body

  -- Use case 2: embedded in Studio.ui as an editor component:
  runUI Studio.ui unit body
