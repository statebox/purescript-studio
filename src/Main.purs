module Main where

import Prelude
import Control.MonadZero (empty)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import ExampleData as Ex
import View.Diagram.DiagramEditor as DiagramEditor
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Studio as Studio

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody

  -- Two use cases for PetrinetEditor:
  --
  -- 1) As a standalone viewer; in this case we want a simple way to pass in an initial net.
  -- 1) As an editor component in some larger framework; don't need initial net; use LoadNet instead.

  -- -- Use case 1: Standalone viewer:
  -- runUI (PetrinetEditor.ui { net: Ex.net1, netApi: Ex.netApi1 }) unit body

  -- Use case 2: embedded in Studio.ui as an editor component:
  runUI Studio.ui unit body

  -- Use case 99: standalone string diagram editor
  -- runUI DiagramEditor.ui unit body
