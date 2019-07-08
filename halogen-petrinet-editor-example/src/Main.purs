module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import ExampleData as Ex
import Record (union)

import View.Petrinet.PetrinetEditor as PetrinetEditor

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI PetrinetEditor.ui (Ex.netInfo1 `union` { roleInfos: [], types: [] }) body
