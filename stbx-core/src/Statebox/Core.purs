module Statebox.Core where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either (..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, message)

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

-- error cases obtained from calling `Stbx.decode`
data DecodeError
  = MissingRequiredField String
  | InvalidHexString
  | IndexOutOfRange String
  | InvalidWireType String
  | Other String

instance showDecodeError :: Show DecodeError where
  show = case _ of
    MissingRequiredField s -> "Missing required field: " <> s
    InvalidHexString       -> "Invalid Hex String"
    IndexOutOfRange s      -> "Invalid out of range: " <> s
    InvalidWireType s      -> "Invalid wire type: " <> s
    Other s                -> "Other: " <> s

derive instance eqDecodeError :: Eq DecodeError

errorToSingleDecodeError :: Regex -> (String -> DecodeError) -> Error -> Maybe DecodeError
errorToSingleDecodeError regex constructor error =
  const (constructor $ message error) <$> match regex (message error)

errorToDecodeError :: Error -> DecodeError
errorToDecodeError error =
  errorMatchers # either (const $ Other "Error in regex definition.")
                         (foldr tryMatch Nothing >>> fromMaybe (Other $ message error))
  where
    errorMatchers = sequence [ (_ /\ MissingRequiredField)     <$> regex "^missing required"   ignoreCase
                             , (_ /\ InvalidHexString # const) <$> regex "^invalid hex string" ignoreCase
                             , (_ /\ IndexOutOfRange)          <$> regex "^index out of range" ignoreCase
                             , (_ /\ InvalidWireType)          <$> regex "^invalid wire type"  ignoreCase
                             ]
    tryMatch (regex /\ errorConstructor) previous = previous <|> errorToSingleDecodeError regex errorConstructor error

--------------------------------------------------------------------------------

foreign import decodeEither :: forall a. HexStr -> (Error -> a) -> (StbxObj -> a) -> a

foreign import stbxObjToJsonString :: StbxObj -> String

decodeToJsonString :: HexStr -> DecodeError \/ String
decodeToJsonString hexStr = stbxObjToJsonString <$> decode hexStr

decode :: HexStr -> DecodeError \/ StbxObj
decode hexStr = decodeEither hexStr (Left <<< errorToDecodeError) Right

--------------------------------------------------------------------------------

foreign import hash :: HexStr -> HashStr
