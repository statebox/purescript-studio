module Test.Common where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

succeed :: forall m. MonadThrow Error m => m Unit
succeed = pure unit
