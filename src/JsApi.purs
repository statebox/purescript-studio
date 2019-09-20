module JsApi where

import Prelude
import Data.Variant as Variant
import Data.Variant (Variant)
import Data.Symbol (SProxy(..))

-- | These commands can easily be sent from JavaScript.
type Command = Variant
  ( setPixels    :: { pixels  :: String
                    , context :: String
                    }
  , setShowWires :: Boolean
  )

setPixels :: String -> String -> Command
setPixels pixels context = Variant.inj (SProxy :: SProxy "setPixels") { pixels, context }

setShowWires :: Boolean -> Command
setShowWires = Variant.inj (SProxy :: SProxy "setShowWires")

showCommand :: Command -> String
showCommand = Variant.match
  { setPixels:    \{pixels, context} -> pixels <> "; " <> context
  , setShowWires: \showWires         -> show showWires
  }
