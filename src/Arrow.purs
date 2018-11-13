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

svgArrow :: forall p i. Vec2D -> Vec2D -> HTML p i
svgArrow q p =
  SE.line
    [ SA.class_ "css-arrow"
    , SA.x1 p'.x
    , SA.y1 p'.y
    , SA.x2 q'.x
    , SA.y2 q'.y
    , SA.markerEnd $ "url(#" <> markerTag <> ")"
    ]
  where
    p' = placeLinePoint q p
    q' = transitionLinePoint q p

arrowHead :: forall p i. HTML p i
arrowHead = SE.defs []
  [ SE.marker
    [ SA.id markerTag
    , SA.markerWidth 6.0
    , SA.markerHeight 6.0
    , SA.strokeWidth 0.0
    , SA.refX 1.6
    , SA.refY 2.0
    , SA.orient "auto"
    , SA.markerUnits "strokeWidth"
    ]
    [ SE.path [ SA.d $ SA.Abs <$> [ SA.M 0.0 0.0, SA.L 0.0 4.0, SA.L 2.0 2.0, SA.Z ] ]
    ]
  ]

placeLinePoint :: Vec2D -> Vec2D -> Vec2D
placeLinePoint p t = { x: p.x + px, y: p.y + py }
  where
    dx = t.x - p.x
    dy = t.y - p.y
    α  = atan2 dy dx
    px = cos α * placeRadius
    py = sin α * placeRadius

transitionLinePoint :: Vec2D -> Vec2D -> Vec2D
transitionLinePoint p t = { x: t.x - tx, y: t.y - ty }
  where
    dx = t.x - p.x
    dy = t.y - p.y
    u  = max (abs dx) (abs dy)
    tx = dx / u * placeRadius
    ty = dy / u * placeRadius

--------------------------------------------------------------------------------

markerTag :: String
markerTag = "arrow"

sqr :: Number -> Number
sqr x = x * x
