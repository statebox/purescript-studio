module Statebox.Core.Initial
  ( Initial
  ) where

import Statebox.Core.Common (Message)

-- | Initial (aka root or namespace or genesis) transaction data.
type Initial =
  { message :: Message
  }
