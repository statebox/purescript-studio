module Statebox.Core.Types where

type HexStr = String

type PathElem = Int

--------------------------------------------------------------------------------

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
