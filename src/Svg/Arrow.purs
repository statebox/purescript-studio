module Svg.Arrow where

import Prelude
import Data.Vec2D (Vec2D)
import Halogen.HTML (HTML)
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord

placeRadius :: Number
placeRadius = 4.0 * tokenRadius

tokenRadius :: Number
tokenRadius = 1.0

arrowHeadHeight :: Number
arrowHeadHeight = 1.8

sqr :: Number -> Number
sqr x = x * x

arrowHead :: forall p i. HTML p i
arrowHead = SE.defs []
  [ SE.marker
    [ SA.id "arrow"
    , SA.markerWidth 10.0
    , SA.markerHeight 10.0
    , SA.strokeWidth 0.2
    , SA.refX 0.0
    , SA.refY 3.0
    , SA.orient "auto"
    , SA.markerUnits "strokeWidth"
    ]
    [ SE.path [ SA.d $ SA.Abs <$> [ SA.M 0.0 0.0, SA.L 0.0 6.0, SA.L 3.0 3.0, SA.Z ]
              , SA.fill   $ Just (SA.RGB 100 100 100)
              , SA.stroke $ Just (SA.RGB 200 200 200)
              ]
    ]
  ]

placeLinePoint :: Vec2D -> Vec2D -> Number -> Vec2D
placeLinePoint pV tV r = { x: pV.x + px, y: pV.y + py }
  where
    dx = tV.x - pV.x
    dy = tV.y - pV.y
    α  = atan2 dy dx
    px = cos α * r
    py = sin α * r

transitionLinePoint :: Vec2D -> Vec2D -> Vec2D
transitionLinePoint pV tV = { x: tV.x - tX, y: tV.y - tY }
  where
    r  = 4.3
    dX = tV.x - pV.x
    dY = tV.y - pV.y
    u  = max (abs dX) (abs dY)
    tX = dX / u * r
    tY = dY / u * r

-- \  1 /
--  \  /
-- 3 \/ 4
--   /\
--  /  \
-- /  2 \

arrowTransLinePoint :: Vec2D -> Vec2D -> Vec2D
arrowTransLinePoint v1 v2 =
  let
    l  = 4.3
    p1  = v1.x
    p2  = v1.y
    s1 = v2.x
    s2 = v2.y

    t = arrowHeadHeight

    ll = sqrt (sqr(s2 - p2) + sqr(p1 - s1))

  in
    if (abs (s2 - p2) >= abs (p1 - s1)) then
      if (s2 - p2) >= 0.0 then -- 1
        let
          xs = (((p1 - s1) / (s2 - p2)) * (-s2 + p2 + l)) + p1 + (t * ((p1 - s1) / ll))
          ys = s2 - l - t * ((s2 - p2) / ll)
        in
          { x: xs, y: ys }
      else -- 2
        let
          xs = (((p1 - s1) / (s2 - p2)) * (-s2 + p2 - l)) + p1 + (t * ((p1 - s1) / ll))
          ys = s2 + l - t * ((s2 - p2) / ll)
        in
          { x: xs, y: ys }
    else
      if (p1 - s1) > 0.0 then --4
        let
          xs = (s1 + l) + (t * ((p1 - s1) / ll))
          ys = -(((s2 - p2) / (p1 - s1)) * (s1 + l - p1)) + p2 + t * ((p1 - s1) / ll)
        in
          { x: xs, y: ys }
      else -- 3
        let
          xs = (s1 - l) + (t * ((p1 - s1) / ll))
          ys = -(((s2 - p2) / (p1 - s1)) * (s1 - l - p1)) + p2 + t * ((p1 - s1) / ll)
        in
          { x: xs, y: ys }

transToPlace :: forall i p. Vec2D -> Vec2D ->  HTML i p
transToPlace p q = SE.line
  let
    p' = transitionLinePoint p q
    q' = placeLinePoint p q (placeRadius + 2.0)
  in
    [ SA.x1 p'.x
    , SA.y1 p'.y
    , SA.x2 q'.x
    , SA.y2 q'.y
    , SA.stroke $ Just (SA.RGB 200 200 200)
    , SA.strokeWidth 0.5
    , SA.markerEnd "url(#arrow)"
    ]

placeToTrans :: forall i p. Vec2D -> Vec2D ->  HTML i p
placeToTrans q p = SE.line
  let
    p' = placeLinePoint p q (placeRadius + 0.5)
    q' = arrowTransLinePoint p q
  in
    [ SA.x1 p'.x
    , SA.y1 p'.y
    , SA.x2 q'.x
    , SA.y2 q'.y
    , SA.stroke $ Just (SA.RGB 200 200 200)
    , SA.strokeWidth 0.5
    , SA.markerEnd "url(#arrow)"
    ]
