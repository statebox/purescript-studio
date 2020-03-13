module KDMonCat.JsApi where

import Prelude
import Data.Variant as Variant
import Data.Variant (Variant)
import Data.Symbol (SProxy(..))

import View.KDMonCat.App (Input) as App

-- | These commands form a Variant, which makes them easy to send from JavaScript to the component.
type Command = Variant
  ( setInput     :: App.Input
  , setShowWires :: Boolean
  )

setInput :: String -> String -> Command
setInput pixels context = Variant.inj (SProxy :: SProxy "setInput") { pixels, context }

setShowWires :: Boolean -> Command
setShowWires = Variant.inj (SProxy :: SProxy "setShowWires")

showCommand :: Command -> String
showCommand = Variant.match
  { setInput:     \{pixels, context} -> pixels <> "; " <> context
  , setShowWires: \showWires         -> show showWires
  }
