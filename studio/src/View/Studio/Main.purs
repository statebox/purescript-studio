module View.Studio.Main where

import Prelude
import Control.Coroutine (consumer)
import Data.Either (either)
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

import View.Model
import View.Studio as Studio
import View.Studio.Model.Route (RouteF(Home), codex)

import ExampleData as Ex

type API =
  { addProject :: String -> ProjectJS -> Aff (Maybe Unit)
  }

type EventHandler =
  { onProjectUpserted :: String -> ProjectJS -> Effect Unit
  , onProjectDeleted :: String -> Effect Unit
  }


main :: User -> EventHandler -> (API -> Effect Unit) -> Effect Unit
main user eventHandler onAPIReady = runAff_ (either (show >>> log) onAPIReady) do
  -- liftEffect $ log $ "studio: ex project: " <> Ex.project1
  body <- awaitBody

  nav <- liftEffect $ makeInterface
  { path } <- liftEffect $ nav.locationState
  let initialRoute = either (const Home) identity $ parse codex path

  io <- runUI Studio.ui (initialState initialRoute nav) body

  -- listen to changes in the url (from f.e. back button)
  _ <- liftEffect $ nav # matchesWith (parse codex)
    \_ newRoute -> launchAff_ $ io.query $ H.tell $ Studio.Navigate newRoute

  io.subscribe $ consumer $ \output -> do
    _ <- liftEffect $ case output of
      Studio.ProjectChanged projectId (Just project) -> eventHandler.onProjectUpserted projectId $ toProjectJS user project
      Studio.ProjectChanged projectId Nothing        -> eventHandler.onProjectDeleted projectId
    pure Nothing

  -- liftEffect $ eventHandler.onProjectUpserted "Example1" $ toProjectJS user Ex.project1
  -- liftEffect $ eventHandler.onProjectUpserted "Example2" $ toProjectJS user Ex.project2

  pure
    { addProject: \projectId project -> io.query $ H.tell (Studio.AddProject projectId $ fromProjectJS project)
    }
  where
    initialState route nav =
      { title:       "Statebox Studio"
      , msg:         "Welcome to Statebox Studio!"
      , projects:    mempty
      , hashSpace:   mempty
      , apiUrl:      Ex.endpointUrl
      , route
      , nav
      , menuItems:   [ "Home"    /\ Just Home
                     , "Project" /\ Nothing
                     , "Help"    /\ Nothing
                     ]
      }
