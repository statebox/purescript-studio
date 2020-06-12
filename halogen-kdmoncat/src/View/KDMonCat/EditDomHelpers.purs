module View.KDMonCat.EditDomHelpers where

import Prelude
import Effect (Effect)

foreign import copyToClipboard :: String -> Effect Unit
foreign import insertText :: String -> Effect Unit
