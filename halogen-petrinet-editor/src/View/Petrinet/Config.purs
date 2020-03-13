module View.Petrinet.Config where

import Prelude
import Svg.Attributes (Duration, seconds)

netScale :: Number
netScale = 100.0

-- TODO #141 this hardcoded constant should be eliminated because the number should depend on other factors
bipartiteLayoutScale :: Number
bipartiteLayoutScale = 10.0

fontSize :: Number
fontSize = 0.16

tokenPadding :: Number
tokenPadding = placeRadius * 0.2

placeRadius :: Number
placeRadius = 4.0 * tokenRadius

tokenRadius :: Number
tokenRadius = 0.5

transitionHeight :: Number
transitionHeight = 2.0 * placeRadius

transitionWidth :: Number
transitionWidth  = 2.0 * placeRadius

arcAnimationDurationSec :: Number
arcAnimationDurationSec = 0.3

arcAnimationDuration :: Duration
arcAnimationDuration = seconds arcAnimationDurationSec
