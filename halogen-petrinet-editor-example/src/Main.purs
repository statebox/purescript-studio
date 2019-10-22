module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Halogen.Aff (awaitLoad, runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Record (union)
import Web.DOM.ParentNode (QuerySelector(..))

import ExampleData as Ex
import View.Petrinet.PetrinetEditor as PetrinetEditor

main :: Effect Unit
main = runHalogenAff do
  awaitLoad

  let net1 = Ex.netInfo1 `union` { roleInfos: [], types: [] }
  net1ElemMaybe <- selectElement (QuerySelector "#net1-container")
  _ <- runUI (PetrinetEditor.ui (Just "net1")) net1 `traverse` net1ElemMaybe

  let net2 = Ex.netInfo2 `union` { roleInfos: [], types: [] }
  net2ElemMaybe <- selectElement (QuerySelector "#net2-container")
  runUI (PetrinetEditor.ui (Just "net2")) net2 `traverse` net2ElemMaybe
