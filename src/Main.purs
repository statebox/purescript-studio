module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Svg.Elements as SE
import Svg.Attributes as SA

import ExampleData as Ex
import ExampleData as Net
import Data.Petrinet.Representation.Dict
import ExampleData (TID)
import PetrinetView as PetrinetView

initialState :: PetrinetView.State TID
initialState =
  { net:    Ex.net1
  , netApi: Ex.netApi1
  , msg:    "No transition clicked"
  }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI (PetrinetView.ui initialState) unit body
