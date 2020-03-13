module Statebox.Browser.Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Hash as Routing

import View.Studio as Studio
import View.Studio (Query(LoadTransactionsThenView))
import View.Studio.Model.Route (RouteF(Home))

import ExampleData as Ex

main :: Effect Unit
main = runHalogenAff do
  urlHash <- liftEffect Routing.getHash
  liftEffect $ log $ "tx browser: transaction hash to be visited: " <> urlHash
  body <- awaitBody
  io <- runUI Studio.ui initialState body
  _ <- io.query $ H.tell (LoadTransactionsThenView Ex.endpointUrl urlHash)
  pure io
  where
    initialState :: Studio.Input
    initialState =
      { title:       "Statebox Transaction Browser"
      , msg:         "Welcome to the Statebox Transaction Browser!"
      , projects:    mempty
      , hashSpace:   mempty
      , apiUrl:      Ex.endpointUrl
      , route:       Home
      }
