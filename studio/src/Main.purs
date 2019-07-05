module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import View.Studio as Studio

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI Studio.ui unit body
