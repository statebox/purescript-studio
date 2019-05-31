module Statebox.Core.Types where

type HexStr = String

type PathElem = Int

--------------------------------------------------------------------------------

-- | About how wirings are encoded:
-- |
-- | 1) The root (top-level) diagram is always `diagrams[0]`.
-- |
-- | 2) The elements of `labels: [0,0]` are the labels of this root diagram, and they are indices
-- |    into the list `(nets <> diagrams)`.
type Wiring =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array Int
  }

type Net =
  { name       :: String
  , partition  :: Array Int
  , names      :: Array String          -- ^ transition names
--, placeNames :: Maybe (Array String)  -- ^ TODO issue #140
  }

type Diagram =
  { name   :: String
  , width  :: Int
  , pixels :: Array Int
  , names  :: Array String
  }

--------------------------------------------------------------------------------

type Firing =
  { execution :: HexStr
  , path      :: Array PathElem
  }
