module View.GridKit.Canvas where

import Prelude

import Data.Array ((..), filter)
import Data.Int (floor, ceil, toNumber)
import Data.Vec3 (Vec2, _x, _y, origin2, point2, Box, boxSize, boxCenter)
import Data.Vec3.AffineTransform
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map, div)
import Math (log, pow, ln10, round, sqrt)
import Svg.Elements as S
import Svg.Attributes hiding (path) as S

import View.ReactiveInput as RI


type Input =
  { gridSpacing :: Number
  , range :: Box Number
  , size :: Vec2 Number
  }

data VoidF a
type Slot = H.Slot VoidF Void

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui =
  RI.mkComponent
    { initialState: {}
    , render
    , handleAction: \_ -> pure unit
    , handleInput: \_ -> pure unit
    }

render :: ∀ m. Input -> {} -> H.ComponentHTML Void () m
render { gridSpacing, range, size } _ =
  S.svg [ S.width (_x size), S.height (_y size) ]
  [ S.g [ S.attr (AttrName "class") "grid grid-v" ] $
      gridLines spacing (_x topLeft) (_x bottomRight)
        # map \{ width, pos } -> let x = m2c_x pos in S.line [ S.strokeWidth width, S.x1 x, S.y1 0.0, S.x2 x, S.y2 (_y size) ]
  , S.g [ S.attr (AttrName "class") "grid grid-h" ] $
      gridLines spacing (_y topLeft) (_y bottomRight)
        # map \{ width, pos } -> let y = m2c_y pos in S.line [ S.strokeWidth width, S.x1 0.0, S.y1 y, S.x2 (_x size), S.y2 y ]
  ]
  where
    scaleXY = size / boxSize range
    scaleMin = min (_x scaleXY) (_y scaleXY)
    spacing = gridSpacing / scaleMin
    rangeCenter = boxCenter range
    canvasCenter = size / pure 2.0
    canvas2model = translate rangeCenter * scale (1.0 / scaleMin) * translate (-canvasCenter)
    model2canvas = translate canvasCenter * scale scaleMin * translate (-rangeCenter)
    m2c_x x = _x (model2canvas `transform` point2 x 0.0)
    m2c_y y = _y (model2canvas `transform` point2 0.0 y)
    topLeft = canvas2model `transform` origin2
    bottomRight = canvas2model `transform` (origin2 + size)

type GridLine = { pos :: Number, width :: Number }
gridLines :: Number -> Number -> Number -> Array GridLine
gridLines spacing start end
  = (ceil (start / stepSize) .. floor (end / stepSize))
  # map (\n -> { pos: toNumber n * stepSize, width: w n - thresholdWidth })
  # filter (\{ width } -> width > 0.0)
  where
    stepSize = pow 10.0 (round (log (spacing / stepMultiplyer) / ln10))
    zoom = stepSize / spacing
    w 0 = maxLineWidth
    w n =
      if n `mod` 5 /= 0 then onesWidth * zoom else
      if n `mod` 10 /= 0 then fivesWidth * zoom else
      min maxLineWidth (10.0 * w (n `div` 10))
    onesWidth = 0.3
    fivesWidth = 1.0
    maxLineWidth = 2.5
    -- the pixel width below the grid lines become hidden
    -- we subtract the threshold so the lines fade in/out instead of suddenly appearing/disappearing
    thresholdWidth = 0.1
    -- Increase the number of lines just when the fives-lines are appearing:
    stepMultiplyer = fivesWidth / thresholdWidth / sqrt(10.0)
