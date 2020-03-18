module Statebox.Browser.Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash as Routing
import Routing.PushState (makeInterface, matchesWith)

import View.Studio as Studio
import View.Studio (Query(..))
import View.Studio.Model.Route

import ExampleData as Ex

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  nav <- liftEffect $ makeInterface
  io <- runUI Studio.ui (initialState (TxHome Nothing) nav) body

  -- listen to changes in the url (from f.e. back button)
  _ <- liftEffect $ nav # matchesWith (parse codex)
    \_ newRoute -> launchAff_ $ io.query $ H.tell $ Studio.Navigate newRoute

  { path } <- liftEffect $ nav.locationState
  case parse codex path of
    Right Home -> do
      _ <- io.query $ H.tell (Navigate (TxHome Nothing))
      urlHash <- liftEffect $ Routing.getHash
      loadTransactionsThenView io Ex.endpointUrl urlHash
    Right (TxHome (Just hash)) -> loadTransactionsThenView io Ex.endpointUrl hash
    Right (ApiRoute (NamespaceR hash) endpointUrl) -> loadTransactionsThenView io endpointUrl hash
    Right (ApiRoute (WiringR    hash) endpointUrl) -> loadTransactionsThenView io endpointUrl hash
    Right (ApiRoute (FiringR    hash) endpointUrl) -> loadTransactionsThenView io endpointUrl hash
    Right (ApiRoute (DiagramR   hash _ _) endpointUrl) -> loadTransactionsThenView io endpointUrl hash
    Right (ApiRoute (NetR       hash _ _) endpointUrl) -> loadTransactionsThenView io endpointUrl hash
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
