module Statebox.Core.Diagram where

-- | See https://docs.statebox.org/spec/nlldiagrams.
type Diagram =
  { name   :: String
  , width  :: Int          -- ^ width of the brick diagram
  , pixels :: Array Int    -- ^ actual brick diagram encoding
  , names  :: Array String
  }
