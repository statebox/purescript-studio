module View.Studio.Main where

import Prelude
import Control.Coroutine
import Effect (Effect)
import Effect.Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Maybe
import Data.Either (either)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Hash as Routing
import Debug.Trace (spy)

import View.Model (Project)
import View.Studio as Studio
import View.Studio (Query(LoadTransactionsThenView))
import View.Studio.Model.Route (RouteF(Home))

import ExampleData as Ex

type User =
  { email :: String
  , uid :: String
  }

type API =
  { addProject :: Project -> Aff (Maybe Unit)
  }

type EventHandler =
  { onProjectCreated :: Project -> Effect Unit
  , onProjectDeleted :: String -> Effect Unit
  }

main :: User -> EventHandler -> (API -> Effect Unit) -> Effect Unit
main user eventHandler onAPIReady = runAff_ (either (show >>> log) onAPIReady) do
  -- liftEffect $ log $ "studio: ex project: " <> Ex.project1
  body <- awaitBody
  io <- runUI Studio.ui initialState body
  io.subscribe $ consumer $ \output -> do
    _ <- liftEffect $ case spy "event" output of
      Studio.ProjectCreated project -> eventHandler.onProjectCreated Ex.project1 -- project
      Studio.ProjectDeleted projectName -> eventHandler.onProjectDeleted projectName
    pure Nothing
  pure
    { addProject: \project -> io.query $ H.tell (Studio.AddProject Ex.project1)
    }
  where
    initialState :: Studio.Input
    initialState =
      { title:       "Statebox Studio"
      , msg:         "Welcome to Statebox Studio!"
      , projects:    mempty -- Ex.projects
      , hashSpace:   mempty
      , apiUrl:      Ex.endpointUrl
      , route:       Home
      }
