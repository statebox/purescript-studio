module Statebox.Console.Main where

import Prelude
import Data.Maybe
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Statebox.Console as Console

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  io <- runUI Console.ui initialState body
  _ <- io.query $ H.tell $ Console.DoAction Console.FetchStuff
  pure io
  where
    initialState :: Console.State
    initialState = { customer: Nothing
                   , paymentMethods: mempty
                   , accounts: [ { invoices: mempty } ]
                   , status: Console.Ok
                   }
