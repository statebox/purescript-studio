module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.Maybe
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.PushState as RoutingP

import View.Studio as Studio
import View.Studio.Model.Route as Route

main :: Effect Unit
main = runHalogenAff do
  pushStateInterface <- H.liftEffect $ RoutingP.makeInterface
  body <- awaitBody
  io <- runUI Studio.ui unit body
  void $ H.liftEffect $ runRouter io pushStateInterface
  pure unit

runRouter :: Route.IO -> RoutingP.PushStateInterface -> Effect Unit
runRouter io psi = void $ RoutingP.matchesWith Route.parse (navigate io) psi

navigate :: Route.IO -> Maybe Route.URLRoute -> Route.URLRoute -> Effect Unit
navigate io old new = when (old /= Just new) $
  launchAff_ $ io.query (H.tell $ Route.Navigate new)