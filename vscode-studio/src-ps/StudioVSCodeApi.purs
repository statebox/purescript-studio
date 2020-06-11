-- If nothing else, this module should force its downstream modules to be compiled.
module StudioVSCodeApi
  ( either
  , Maybe(..)

  , parseNet
  , mkParseResult
  , mkTransitionSymbols
  , toNetWithDefaultName
  ) where

import Data.Maybe as Data.Maybe
import Data.Either as Data.Either

import Language.Statebox as LS
import Language.Statebox.Net.Generator as LSNG
import Language.Statebox.Net.Generator.Net as LSNGN

--------------------------------------------------------------------------------

type Maybe = Data.Maybe.Maybe
either = Data.Either.either

--------------------------------------------------------------------------------

parseNet = LS.parseNet
mkParseResult = LSNG.mkParseResult
mkTransitionSymbols = LSNG.mkTransitionSymbols
toNetWithDefaultName = LSNGN.toNetWithDefaultName
