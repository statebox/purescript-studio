module View.CopyToClipboard where

import Prelude
import Effect (Effect)

foreign import copyToClipboard :: String -> Effect Unit