module Statebox.Core.Common
  ( HexStr
  , Message
  , Singleton
  , TxId
  ) where

import Data.NonEmpty (NonEmpty)

-- | This tags an Array that is expected (but not guaranteed) to have exactly one element. (TODO: newtype.)
type Singleton = NonEmpty Array

type TxId = String

type HexStr = String

-- | Some transaction data has a 'message' field, and they have this type.
type Message = String
