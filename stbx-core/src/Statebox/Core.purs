module Statebox.Core where

import Prelude
import Effect
import Data.Either (Either (..))
import Data.Either.Nested (type (\/))
import Effect.Exception (Error)

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)
import Statebox.Core.Transaction.Codec (DecodeError, errorToDecodeError)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

-- TODO In what sense is this effectful? Let's reify exceptions here.
foreign import decode :: HexStr -> Effect StbxObj

foreign import decodeEither :: forall a. HexStr -> (Error -> a) -> (StbxObj -> a) -> a

foreign import stbxObjToJsonString :: StbxObj -> String

decodeToJsonString :: HexStr -> DecodeError \/ String
decodeToJsonString hexStr = stbxObjToJsonString <$> decodeFoo hexStr

decodeFoo :: HexStr -> DecodeError \/ StbxObj
decodeFoo hexStr = decodeEither hexStr (Left <<< errorToDecodeError) Right

--------------------------------------------------------------------------------

foreign import hash :: HexStr -> HashStr
