module Statebox.Console.Main where

import Prelude hiding ((/))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (RouteDuplex', path, root, segment, int, optional, param)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax
import Routing.PushState as Routing.PushState

import Statebox.Console as Console
import Statebox.Console (Route(..))

import ExampleData as ExampleData
import View.Model (ProjectId, Project) -- TODO rm, used to define example data

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  pushStateInterface <- liftEffect Routing.PushState.makeInterface
  io <- runUI Console.ui initialState body
  _ <- io.query $ H.tell $ Console.DoAction Console.FetchStuff
  pure io
  where
    initialState :: Console.State
    initialState = { route: Home
                   , customer: Nothing
                   , paymentMethods: mempty
                   , subscriptions: mempty
                   , plans: mempty
                   , accounts: [ { invoices: mempty } ]
                   , projects: exampleProjects
                   -- , projects: [ ExampleData.project1 ]
                   , status: Console.Ok
                   }


routesCodex :: RouteDuplex' Route
routesCodex = root $ sum
  { "Home":         noArgs
  , "ProjectR":     "project" / segment
  , "Projects":     "project" / noArgs
  , "APIKeys":      "key" / noArgs
  , "Account":      "account" / noArgs
  , "Invoices":     "invoices" / segment
  , "Subscription": "subscriptions" / noArgs
  , "Plan":         "plans" / noArgs
  }

--------------------------------------------------------------------------------

exampleProjects :: Map ProjectId Project
exampleProjects = Map.fromFoldable
  [ "project1" /\ ExampleData.project1
  ]
