module Main where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen (HalogenIO)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

import Example as Example


main :: Effect Unit
main = log "main: loaded."

-- run :: ∀ f. {} -> String -> Aff (Maybe (HalogenIO f Void Aff))
run {} selector = do
  elemMaybe <- selectElement (QuerySelector selector)
  runUI Example.ui {} `traverse` elemMaybe
