module Statebox.Browser.Main where

import Prelude
import Data.Either (either)
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface)

import View.Studio as Studio
import View.Studio (Query(LoadTransactionsThenView))
import View.Studio.Model.Route

import ExampleData as Ex

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody

  nav <- liftEffect $ makeInterface
  { path } <- liftEffect $ nav.locationState
  let initialRoute' = either (const $ TxHome Nothing) identity $ parse codex path
  let initialRoute = if initialRoute' == Home then TxHome Nothing else initialRoute'

  io <- runUI Studio.ui (initialState initialRoute nav) body

  case initialRoute of
    TxHome (Just hash) -> loadTransactionsThenView io Ex.endpointUrl hash
    ApiRoute (NamespaceR hash) endpointUrl -> loadTransactionsThenView io endpointUrl hash
    ApiRoute (WiringR    hash) endpointUrl -> loadTransactionsThenView io endpointUrl hash
    ApiRoute (FiringR    hash) endpointUrl -> loadTransactionsThenView io endpointUrl hash
    ApiRoute (DiagramR   hash _ _) endpointUrl -> loadTransactionsThenView io endpointUrl hash
    ApiRoute (NetR       hash _ _) endpointUrl -> loadTransactionsThenView io endpointUrl hash
    _ -> pure unit

  pure io
  where
    loadTransactionsThenView io endpointUrl hash = do
      liftEffect $ log $ "tx browser: transaction hash to be visited: " <> hash
      _ <- io.query $ H.tell (LoadTransactionsThenView endpointUrl hash)
      pure unit

    initialState route nav =
      { title:       "Statebox Transaction Browser"
      , msg:         "Welcome to the Statebox Transaction Browser!"
      , projects:    mempty
      , hashSpace:   mempty
      , apiUrl:      Ex.endpointUrl
      , route
      , nav
      , menuItems:   [ "Home" /\ Just (TxHome Nothing) ]
      }
