module Arrow where

import Prelude (max, negate, ($), (*), (+), (-), (/), (<$>), (>), (>=), (<>))
import Config
import Data.Vec2D (Vec2D)
import Halogen.HTML (HTML)
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

transToPlace :: forall p i. Vec2D -> Vec2D -> HTML p i
transToPlace p q =
  SE.line
    [ SA.class_ "css-tp"
    , SA.x1 p'.x
    , SA.y1 p'.y
    , SA.x2 q'.x
    , SA.y2 q'.y
    , SA.markerEnd $ "url(#" <> markerTag <> ")"
    ]
  where
    p' = transitionLinePoint q p (2.0)
    q' = placeLinePoint q p (placeRadius + 1.5)

placeToTrans :: forall p i. Vec2D -> Vec2D -> HTML p i
placeToTrans q p =
  SE.line
    [ SA.class_ "css-pt"
    , SA.x1 p'.x
    , SA.y1 p'.y
    , SA.x2 q'.x
    , SA.y2 q'.y
    , SA.markerEnd $ "url(#" <> markerTag <> ")"
    ]
  where
    p' = placeLinePoint q p (placeRadius)
    q' = transitionLinePoint q p (3.5)

arrowHead :: forall p i. HTML p i
arrowHead = SE.defs []
  [ SE.marker
    [ SA.id markerTag
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

transitionLinePoint :: Vec2D -> Vec2D -> Number -> Vec2D
transitionLinePoint pV tV r = { x: tV.x - tX, y: tV.y - tY }
  where
    dX = tV.x - pV.x
    dY = tV.y - pV.y
    u  = max (abs dX) (abs dY)
    tX = dX / u * r
    tY = dY / u * r

--------------------------------------------------------------------------------

markerTag :: String
markerTag = "arrow"

sqr :: Number -> Number
sqr x = x * x
