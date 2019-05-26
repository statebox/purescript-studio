module Statebox.Core where

import Prelude
import Effect

import Statebox.Core.Types (HexStr)

-- TODO is this actually effectful?
foreign import decode :: HexStr -> Effect String
