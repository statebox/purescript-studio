module Config where

import Prelude
import Svg.Attributes (Duration, DurationF(..), seconds, FillState(Freeze), FontSize(..), CSSLength(..))

placeRadius :: Number
placeRadius = 4.0 * tokenRadius

tokenRadius :: Number
tokenRadius = 0.5

transitionHeight :: Number
transitionHeight = 2.0 * placeRadius

transitionWidth :: Number
transitionWidth  = 2.0 * placeRadius

arcAnimationDuration :: Duration
arcAnimationDuration = seconds 0.70

arrowHeadHeight :: Number
arrowHeadHeight = 1.8
