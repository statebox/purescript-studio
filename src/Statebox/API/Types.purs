module Statebox.API.Types where

type HexStr = String

type URL = String

type PathElem = Int

--------------------------------------------------------------------------------

type Wiring =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array Int
  }

type Net =
  { name      :: String
  , partition :: Array Int
  , names     :: Array String
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
