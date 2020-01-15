module Statebox.Core where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (caseJsonString)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either (..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, message)

import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)

import Statebox.Core.Execution (StbxObj) -- TODO StbxObj shouldn't be in Execution

-- | error cases obtained from calling `Stbx.decode`
-- | this refers to https://github.com/statebox/stbx-core-js
-- | which calls out to https://github.com/statebox/js-stbx-packet-codec/blob/d25f4945181e19cc8b5fc7db3dfd803cb024bb48/generated/statebox.proto.js#L1493
data DecodeError
  = MissingRequiredField String
  | InvalidHexString
  | IndexOutOfRange String
  | InvalidWireType String
  | Other String

missingRequiredFieldMessage :: String
missingRequiredFieldMessage = "Missing required field: "

invalidHexStringMessage :: String
invalidHexStringMessage = "Invalid Hex String"

indexOutOfRangeMessage :: String
indexOutOfRangeMessage = "Index out of range: "

invalidWireTypeMessage :: String
invalidWireTypeMessage = "Invalid wire type: "

otherMessage :: String
otherMessage = "Other: "

instance showDecodeError :: Show DecodeError where
  show = case _ of
    MissingRequiredField s -> missingRequiredFieldMessage <> s
    InvalidHexString       -> invalidHexStringMessage
    IndexOutOfRange s      -> indexOutOfRangeMessage <> s
    InvalidWireType s      -> invalidWireTypeMessage <> s
    Other s                -> otherMessage <> s

derive instance eqDecodeError :: Eq DecodeError

decodeDecodeErrorCase :: (String -> DecodeError) -> String -> String -> Either String DecodeError
decodeDecodeErrorCase constructor message s = case stripPrefix (Pattern message) s of
  Just s' -> Right $ constructor s'
  Nothing -> Left "Unrecognised string for DecodeError"

instance decodeJsonDecodeError :: DecodeJson DecodeError where
  decodeJson = caseJsonString (Left "DecodeError should be represented by a string") decodeString
    where
      decodeString :: String -> Either String DecodeError
      decodeString s
        =   decodeDecodeErrorCase MissingRequiredField     missingRequiredFieldMessage s
        <|> decodeDecodeErrorCase (const InvalidHexString) invalidHexStringMessage     s
        <|> decodeDecodeErrorCase IndexOutOfRange          indexOutOfRangeMessage      s
        <|> decodeDecodeErrorCase InvalidWireType          invalidWireTypeMessage      s
        <|> decodeDecodeErrorCase Other                    otherMessage                s

errorToSingleDecodeError :: Regex -> (String -> DecodeError) -> Error -> Maybe DecodeError
errorToSingleDecodeError regex constructor error =
  const (constructor $ message error) <$> match regex (message error)

errorMatchers = sequence [ (_ /\ MissingRequiredField)     <$> regex "^missing required"   ignoreCase
                         , (_ /\ InvalidHexString # const) <$> regex "^invalid hex string" ignoreCase
                         , (_ /\ IndexOutOfRange)          <$> regex "^index out of range" ignoreCase
                         , (_ /\ InvalidWireType)          <$> regex "^invalid wire type"  ignoreCase
                         ]

errorToDecodeError :: Error -> DecodeError
errorToDecodeError error =
  errorMatchers # either (const $ Other "Error in regex definition.")
                         (foldr tryMatch Nothing >>> fromMaybe (Other $ message error))
  where
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
