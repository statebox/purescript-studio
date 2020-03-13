module Statebox.Core.Net where

import Data.ArrayMultiset
import Data.Maybe (Maybe)

type Net =
  { name       :: String
  , partition  :: ArrayMultiset PID     -- ^ NLL encoding
  , names      :: Array String          -- ^ transition names
  , placeNames :: Maybe (Array String)
  }

-- | Place id.
type PID = Int

-- TODO newtype
-- | Transition id.
type TID = Int

unTID :: TID -> Int
unTID i = i
