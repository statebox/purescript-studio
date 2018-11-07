module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import ExampleData as Ex
import ExampleData (PID, TID)
import PetrinetView as PetrinetView

initialState :: PetrinetView.StateF PID TID
initialState =
  { net:    Ex.net1
  , netApi: Ex.netApi1
  , msg:    "No transition clicked"
  }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI (PetrinetView.ui (Just "n1") initialState) unit body
