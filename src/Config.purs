module Config where

import Prelude
import Svg.Attributes (Duration, seconds)

<<<<<<< HEAD
fontSize :: Number
fontSize = 0.1

tokenPadding :: Number
tokenPadding = placeRadius * 0.2

=======
>>>>>>> 05e34be6be7516fd8598cbcfda3e82a3520f78c9
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
