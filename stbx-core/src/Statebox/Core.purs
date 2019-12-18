module Statebox.Core where

import Prelude ((<$>), (<<<))
import Data.Either (Either (..))
import Data.Either.Nested (type (\/))
import Effect.Exception (Error)

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)
import Statebox.Core.Transaction.Codec (DecodeError, errorToDecodeError)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

foreign import decodeEither :: forall a. HexStr -> (Error -> a) -> (StbxObj -> a) -> a

foreign import stbxObjToJsonString :: StbxObj -> String

decodeToJsonString :: HexStr -> DecodeError \/ String
decodeToJsonString hexStr = stbxObjToJsonString <$> decode hexStr

decode :: HexStr -> DecodeError \/ StbxObj
decode hexStr = decodeEither hexStr (Left <<< errorToDecodeError) Right

--------------------------------------------------------------------------------

foreign import hash :: HexStr -> HashStr
