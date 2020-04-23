module View.Studio.Main where

import Prelude
import Control.Coroutine (consumer)
import Data.Either (either)
import Data.Maybe
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse, print)
import Routing.PushState (makeInterface, matchesWith)

import View.Model
import View.Studio as Studio
import View.Studio.Model.Route

import ExampleData as Ex

type APIJS =
  { addProject :: String -> ProjectJS -> Aff Unit
  , starterProjectId :: String
  , starterProject :: ProjectJS
  }

type EventHandler =
  { onProjectUpserted :: String -> ProjectJS -> Effect Unit
  , onProjectDeleted :: String -> Effect Unit
  }

main :: User -> EventHandler -> (APIJS -> Effect Unit) -> Effect Unit
main user eventHandler onAPIReady = runAff_ (either (show >>> log) onAPIReady) do
  body <- awaitBody

  nav <- liftEffect $ makeInterface
  { path } <- liftEffect $ nav.locationState
  let starterProjectId = user.uid <> "Starter"
  let initialRoute = if user.isNew then ProjectRoute starterProjectId (KDMonCatR Ex.starterDiagramId)
                                   else either (const Home) identity (parse codex path)

  -- make sure the url matches the initialRoute
  liftEffect $ nav.pushState (unsafeToForeign {}) $ print codex initialRoute

  io <- runUI Studio.ui (initialState initialRoute nav) body

  -- listen to changes in the url, e.g. from the back button
  _ <- liftEffect $ nav # matchesWith (parse codex)
    \_ newRoute -> launchAff_ $ io.query $ H.tell $ Studio.Navigate newRoute

  io.subscribe $ consumer $ \output -> do
    _ <- liftEffect $ case output of
      Studio.ProjectChanged projectId (Just project) -> eventHandler.onProjectUpserted projectId $ toProjectJS user project
      Studio.ProjectChanged projectId Nothing        -> eventHandler.onProjectDeleted projectId
    pure Nothing

  -- optionally load example data
  guard false do
    void $ io.query $ H.tell (Studio.LoadProject (user.uid <> "Example1") Ex.project1)
    void $ io.query $ H.tell (Studio.LoadProject (user.uid <> "Example2") Ex.project2)

  pure
    { addProject: \projectId project ->
        void $ io.query $ H.tell $ Studio.LoadProject projectId $ fromProjectJS project
    , starterProjectId
    , starterProject: toProjectJS user Ex.starterProject
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
      , navEditMode: false
      , menuItems:   [ "Projects" /\ Just Home ]
      }
