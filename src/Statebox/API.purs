module Statebox.API where

import Prelude
import Effect

import Statebox.API.Types (HexStr)

-- TODO is this actually effectful?
foreign import decode :: HexStr -> Effect String
