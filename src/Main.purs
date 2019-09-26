module Main where

import Prelude

import Data.Array.NonEmpty (toArray)
import Data.String.Common (trim)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Global (decodeURI)
import Halogen as H
import Halogen (HalogenIO)
import Halogen.Aff (awaitLoad, runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window) as HTML
import Web.HTML.Location (hash) as Location
import Web.HTML.Window as Window
import Web.HTML.Event.EventTypes (message) as ET
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.MessageEvent as MessageEvent

import View.App (Action(UpdateContext, UpdatePixels))
import View.App as App


initialPixels :: String
initialPixels = """
22211
22233
45533
65577
"""

initialContext :: String
initialContext = """
1: a ->
2: b b b -> a c d
3: c d d -> e
4: b -> c c
5: c c c c c -> d e e e
6: -> c c c
7: e e e -> e
"""

main :: Effect Unit
main = do
  w <- HTML.window
  l <- Window.location w
  h <- Location.hash l
  let input = parseHash h
  runHalogenAff do
    awaitLoad
    
  --   run input "body"

run :: ∀ a. App.Input -> String -> Aff (Maybe (HalogenIO App.Query a Aff))
run input selector = do 
  elemMaybe <- selectElement (QuerySelector selector)
  runUI App.appView input `traverse` elemMaybe

type APIF a = { setPixelsAndContext :: String -> String -> a }

-- | This returns an API to make invocation from JavaScript easier.
runJs1 :: App.Input -> String -> Aff (APIF (Effect Unit))
runJs1 input selector = do
  ioMaybe <- run input selector
  pure { setPixelsAndContext: \pixels ctx -> do
           log "setPixelsAndContext: here"
           p <- maybe (log "setPixelsAndContext: pixels are Nothing") runHalogenAff $ UpdatePixels pixels `performAppAction` ioMaybe
           c <- maybe (log "setPixelsAndContext: context is Nothing") runHalogenAff $ UpdateContext ctx `performAppAction` ioMaybe
           pure unit
       }
  where
    performAppAction :: App.Action -> Maybe (HalogenIO App.Query _ Aff) -> Maybe (Aff (Maybe Unit))
    performAppAction action ioMaybe' = ioMaybe' <#> \io -> io.query <<<  H.tell <<< App.DoAction $ action

-- | Install the component at the specified selector and connect it to an on-message listener on the
-- | window so we can send commands to the component using window.postMessage.
runJs2 :: App.Input -> String -> Aff Unit
runJs2 input selector = do
  api <- runJs1 input selector
  liftEffect do
    log $ "runJs2: pixels, context: " <> input.pixels <> ", " <> input.context
    messageListener <- eventListener $ \e -> do
      log $ "runJs2: event data: " <> (fromMaybe "not a MessageEvent" <<< map MessageEvent.data_ <<< MessageEvent.fromEvent $ e)
      api.setPixelsAndContext input.pixels input.context
      pure unit
    HTML.window >>= Window.toEventTarget
                >>> addEventListener ET.message messageListener false
  pure unit

--------------------------------------------------------------------------------

parseHash :: String -> App.Input
parseHash hash =
  let defaultInput = { pixels: trim initialPixels, context: trim initialContext } in
  regex "pixels=([^&]*)&context=(.*)" ignoreCase # either (\_ -> defaultInput) \re -> 
    match re hash # map toArray # case _ of
      Just [_, Just p, Just c] -> case decodeURI p, decodeURI c of
        Just pixels, Just context -> { pixels, context }
        _, _ -> defaultInput
      _ -> defaultInput
