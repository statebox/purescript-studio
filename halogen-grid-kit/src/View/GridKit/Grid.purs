module View.GridKit.Grid where

import Prelude

import Data.Array ((..), filter)
import Data.Int (floor, ceil, toNumber)
import Data.Vec3 (Vec2, vec2, _x, _y, origin2, point2)
import Data.Vec3.AffineTransform
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map, div)
import Math (log, pow, ln10, round, sqrt)
import Svg.Elements as S
import Svg.Attributes as S

import View.ReactiveInput as RI


type Input =
  { gridSpacing :: Number
  , model2svg :: AffineTransform Number
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
render { gridSpacing, model2svg, size } _ =
  S.g []
  [ S.g [ S.attr (AttrName "class") "grid grid-v" ] $
      gridLines spacing (_x topLeft) (_x bottomRight)
        # map \{ width, pos } -> let x = m2s_x pos in S.line [ S.strokeWidth width, S.x1 x, S.y1 0.0, S.x2 x, S.y2 (_y size) ]
  , S.g [ S.attr (AttrName "class") "grid grid-h" ] $
      gridLines spacing (_y topLeft) (_y bottomRight)
        # map \{ width, pos } -> let y = m2s_y pos in S.line [ S.strokeWidth width, S.x1 0.0, S.y1 y, S.x2 (_x size), S.y2 y ]
  ]
  where
    svg2model = inverse model2svg
    spacing = _x (svg2model `transform` vec2 gridSpacing gridSpacing)
    m2s_x x = _x (model2svg `transform` point2 x 0.0)
    m2s_y y = _y (model2svg `transform` point2 0.0 y)
    topLeft = svg2model `transform` origin2
    bottomRight = svg2model `transform` (origin2 + size)

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
