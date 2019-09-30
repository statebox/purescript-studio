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
import Global (decodeURIComponent)
import Halogen as H
import Halogen (HalogenIO)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window) as HTML
import Web.HTML.Window as Window
import Web.HTML.Event.EventTypes (message) as ET
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.MessageEvent as MessageEvent

import View.App (Action(UpdateContext, UpdatePixels))
import View.App as App


initialPixels :: String
initialPixels = trim """
gggff
ggghh
ijjhh
kjjll
"""

initialContext :: String
initialContext = trim """
f: a ->
g: b b b -> a c d
h: c d d -> e
i: b -> e d
j: [2 3 1 4]
k: -> e e
l: e e e -> e
"""

main :: Effect Unit
main = log "main: kdmoncat bundle loaded."

run :: ∀ a. App.Input -> String -> Aff (Maybe (HalogenIO App.Query a Aff))
run input selector = do
  elemMaybe <- selectElement (QuerySelector selector)
  runUI App.appView input `traverse` elemMaybe

type APIF a = { setInput :: String -> String -> a }

-- | This returns an API to make invocation from JavaScript easier.
runJs1 :: App.Input -> String -> Aff (APIF (Effect Unit))
runJs1 input selector = do
  ioMaybe <- run input selector
  pure { setInput: \pixels ctx -> do
           log "setInput: here"
           p <- maybe (log "setInput: pixels are Nothing") runHalogenAff $ UpdatePixels pixels `performAppAction` ioMaybe
           c <- maybe (log "setInput: context is Nothing") runHalogenAff $ UpdateContext ctx `performAppAction` ioMaybe
           pure unit
       }
  where
    performAppAction :: App.Action -> Maybe (HalogenIO App.Query _ Aff) -> Maybe (Aff (Maybe Unit))
    performAppAction action ioMaybe' = ioMaybe' <#> \io -> io.query <<<  H.tell <<< App.DoAction $ action

-- | Install the component at the specified selector and connect it to an on-message listener on the
-- | window so we can send commands to the component using window.postMessage.
runJs2 :: App.Input -> String -> Aff Unit
runJs2 initialInput selector = do
  api <- runJs1 initialInput selector
  liftEffect do
    log $ "runJs2: initial pixels, context: " <> initialInput.pixels <> ", " <> initialInput.context
    messageListener <- eventListener $ \e -> do
      log $ "runJs2: event data: " <> (fromMaybe "not a MessageEvent" <<< map MessageEvent.data_ <<< MessageEvent.fromEvent $ e)
      let input = initialInput -- TODO get from event
      api.setInput input.pixels input.context
      pure unit
    HTML.window >>= Window.toEventTarget
                >>> addEventListener ET.message messageListener false
  pure unit

--------------------------------------------------------------------------------

parseHash :: String -> App.Input
parseHash hash =
  let defaultInput = { pixels: initialPixels, context: initialContext } in
  regex "pixels=([^&]*)&context=(.*)" ignoreCase # either (\_ -> defaultInput) \re ->
    match re hash # map toArray # case _ of
      Just [_, Just p, Just c] -> case decodeURIComponent p, decodeURIComponent c of
        Just pixels, Just context -> { pixels, context }
        _, _ -> defaultInput
      _ -> defaultInput
