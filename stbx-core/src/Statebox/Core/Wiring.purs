-- TODO reorder this file for clarity
module Statebox.Core.Wiring where

import Prelude
import Data.Newtype (class Newtype, un)

import Statebox.Core.Net (Net)
import Statebox.Core.Diagram (Diagram)

-- | About how wirings are encoded:
-- |
-- | 1) The root (top-level) diagram is always `diagrams[0]`.
-- |
-- | 2) The elements of `labels: [0,0]` are the labels of this root diagram, and they are indices
-- |    into the list `(nets <> diagrams)`.
-- |
-- | for a more detailed description, refer to https://hackmd.io/0CPzJ_V-Qkm0y40NLQRhWw?view#Representing-the-gluing
-- TODO: replace link with link to docs site
type WiringF i =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array i
  }

type WiringRaw = WiringF Int

type Wiring = WiringF NetsAndDiagramsIndex

-- TODO lenses, isos -----------------------------------------------------------

fromWiringRaw :: WiringRaw -> Wiring
fromWiringRaw w = w { labels = NetsAndDiagramsIndex <$> w.labels }

toWiringRaw :: Wiring -> WiringRaw
toWiringRaw w = w { labels = un NetsAndDiagramsIndex <$> w.labels }

--------------------------------------------------------------------------------

-- | An index into a list `nets <> diagrams`, as used by a `Wiring`'s `labels` field.
newtype NetsAndDiagramsIndex = NetsAndDiagramsIndex Int

derive instance newtypeNetsAndDiagramsIndex :: Newtype NetsAndDiagramsIndex _

instance showNetsAndDiagramsIndex :: Show NetsAndDiagramsIndex where
  show (NetsAndDiagramsIndex x) = show x
