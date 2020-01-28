module Statebox.Core.WiringTree where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe)

import Statebox.Core.Types (Diagram, Net, Wiring)

data WiringTree
  = Net Net
  | Diagram Diagram (Array WiringTree)

-- | This function translates a `Wiring` into a `WiringTree`
-- | For the moment, we forget about diagrams and gluings and we consider only simple nets
wiringToWiringTree :: Wiring -> Maybe WiringTree
wiringToWiringTree wiring = Net <$> head wiring.nets
