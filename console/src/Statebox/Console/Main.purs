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
import Statebox.Console (ProjectId, Project) -- TODO remove, used to define example data

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
                   , apiKeys : [ { name: "My API key #1", hex: "01010101", billingAccount: Nothing }
                               , { name: "My API key #2", hex: "02020202", billingAccount: Nothing }
                               ]
                   , projects: exampleProjects
                   , rootTransactions: ["00AA00", "00BB00", "00CC00" ]
                   , status: Console.Ok
                   }


routesCodex :: RouteDuplex' Route
routesCodex = root $ sum
  { "Home":         noArgs
  , "ProjectR":     "project" / segment
  , "Projects":     "project" / noArgs
  , "RootTx":       "tx" / noArgs
  , "APIKeys":      "key" / noArgs
  , "Account":      "account" / segment
  , "Invoices":     "invoices" / segment
  , "Subscription": "subscriptions" / noArgs
  , "Plan":         "plans" / noArgs
  }

--------------------------------------------------------------------------------

exampleProjects :: Map ProjectId Project
exampleProjects = Map.fromFoldable
  [ "project1" /\ { name: "My Project 1"
                  , rootTransactions: [ "0100ABC123", "0100DEF456", "0100GHI789" ]
                  }
  , "project2" /\ { name: "My Project 2"
                  , rootTransactions: [ "0200ABC123", "0200DEF456", "0200GHI789" ]
                  }
  , "project3" /\ { name: "My Project 3"
                  , rootTransactions: [ "0300ABC123", "0300DEF456", "0300GHI789" ]
                  }
  ]
