module Statebox.Core where

import Prelude
import Effect

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

-- TODO In what sense is this effectful? Let's reify exceptions here.
foreign import decode :: HexStr -> Effect StbxObj

foreign import stbxObjToJsonString :: StbxObj -> String

decodeToJsonString :: HexStr -> Effect String
decodeToJsonString = map stbxObjToJsonString <<< decode

--------------------------------------------------------------------------------

foreign import hash :: HexStr -> HashStr
