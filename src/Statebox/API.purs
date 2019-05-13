module Statebox.API where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String.CodePoints (take)
import Effect

import Statebox.API.Types
import Statebox.Transaction

-- TODO is this actually effectful?
foreign import decode :: HexStr -> Effect String

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8

findRootDiagramMaybe :: WiringTx -> Maybe Diagram
findRootDiagramMaybe = Array.head <<< _.wiring.diagrams
