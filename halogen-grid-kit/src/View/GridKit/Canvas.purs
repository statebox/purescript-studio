module View.GridKit.Canvas where

import Prelude

import Data.Array ((..), filter)
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.Int (floor, ceil, toNumber)
import Data.Vec3 (Vec2, _x, _y, Box(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map, div)
import Halogen.HTML.Properties (ref)
import Halogen.Query.Input (RefLabel(..))
import Math (log, pow, ln10, round, sqrt)
import Svg.Elements as S
import Svg.Attributes hiding (path) as S
import Web.DOM.Element (Element, setAttribute)

import View.ReactiveInput as RI

import Debug.Trace

type Input =
  { gridSpacing :: Number
  , range :: Box Number
  , size :: Vec2 Number
  }

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui =
  RI.mkComponent
    { initialState: {}
    , render
    , handleAction: \_ -> pure unit
    , handleInput: \_ -> pure unit
    }

render :: ∀ m. Input -> {} -> H.ComponentHTML Void () m
render { gridSpacing, range: range@(Box { topLeft, bottomRight }), size } _ =
  S.svg [ S.width (_x size), S.height (_y size), viewBox range ]
  [ S.g [ S.attr (AttrName "class") "grid grid-v" ] $
      gridLines gridSpacing ((x2 - x1) / _x size) x1 x2
        # map \{ width, pos } -> S.line [ S.strokeWidth width, S.x1 pos, S.y1 y1, S.x2 pos, S.y2 y2 ]
  , S.g [ S.attr (AttrName "class") "grid grid-h" ] $
      gridLines gridSpacing ((y2 - y1) / _y size) y1 y2
        # map \{ width, pos } -> S.line [ S.strokeWidth width, S.x1 x1, S.y1 pos, S.x2 x2, S.y2 pos ]
  ]
  where
    x1 = _x topLeft
    y1 = _y topLeft
    x2 = _x bottomRight
    y2 = _y bottomRight

type GridLine = { pos :: Number, width :: Number }
gridLines :: Number -> Number -> Number -> Number -> Array GridLine
gridLines pixelSpacing pixelSize start end
  = (ceil (start / stepSize) .. floor (end / stepSize))
  # map (\n -> { pos: toNumber n * stepSize, width: pixelSize * (w n - thresholdWidth) })
  # filter (\{ width } -> width > 0.0)
  where
    spacing = pixelSpacing * pixelSize
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

viewBox :: ∀ r i. Box Number -> IProp (viewBox :: String | r) i
viewBox (Box { topLeft: p0, bottomRight: p1 }) =
  let dp = p1 - p0 in S.viewBox (_x p0) (_y p0) (_x dp) (_y dp)