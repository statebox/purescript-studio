module Statebox.Core where

import Prelude
import Effect
import Data.Either
import Effect.Exception (Error)

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)
import Statebox.Core.Transaction.Codec (DecodeError)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

-- TODO In what sense is this effectful? Let's reify exceptions here.
foreign import decode :: HexStr -> Effect StbxObj

foreign import decodeEither :: forall a. HexStr -> (Error -> a) -> (StbxObj -> a) -> a

foreign import stbxObjToJsonString :: StbxObj -> String

decodeToJsonString :: forall a. HexStr -> Either Error String
decodeToJsonString hexStr = decodeEither hexStr (Left) (Right <<< stbxObjToJsonString)

--------------------------------------------------------------------------------

foreign import hash :: HexStr -> HashStr
