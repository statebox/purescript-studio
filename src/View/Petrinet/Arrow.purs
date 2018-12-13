module View.Petrinet.Arrow where

import Prelude (max, negate, ($), (*), (+), (-), (/), (<$>), (>), (>=), (<>))
import Data.Vec2D (Vec2D)
import Halogen.HTML (HTML)
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

import View.Petrinet.Config

-- | This refers to an `arrowheadMarkerId`, for which a marker must be defined in the `<defs>`.
-- | Such a marker is defined provided here as `svgArrowheadMarker`.
svgPreArrow :: forall p i. Vec2D -> Vec2D -> HTML p i
svgPreArrow src dest =
  SE.line
    [ SA.class_ "css-arrow"
    , SA.x1 src'.x
    , SA.y1 src'.y
    , SA.x2 dest'.x
    , SA.y2 dest'.y
    , SA.markerEnd $ "url(#" <> arrowheadMarkerId <> ")"
    ]
  where
    src'  = placeLinePoint      src dest
    dest' = transitionLinePoint src dest

svgPostArrow :: forall p i. Vec2D -> Vec2D -> HTML p i
svgPostArrow src dest =
  SE.line
    [ SA.class_ "css-arrow"
    , SA.x1 src'.x
    , SA.y1 src'.y
    , SA.x2 dest'.x
    , SA.y2 dest'.y
    , SA.markerEnd $ "url(#" <> arrowheadMarkerId <> ")"
    ]
  where
    src'  = transitionLinePoint dest src
    dest' = placeLinePoint      dest src

-- | An arrowhead shape that can be attached to other SVG elements such as lines and paths.
-- | SVG takes care of rotating this marker so that it will be orientated according to that shape.
svgArrowheadMarker :: forall p i. HTML p i
svgArrowheadMarker =
  SE.marker
    [ SA.id arrowheadMarkerId
    , SA.class_ "css-arrowhead"
    , SA.orient SA.AutoOrient
    , SA.markerUnits SA.StrokeWidth
    , SA.strokeWidth 0.0
    , SA.markerWidth 12.0
    , SA.markerHeight 12.0
    , SA.refX 6.2
    , SA.refY 6.0
    ]
    [ SE.path [ SA.d $ SA.Abs <$> [ SA.M 0.0 0.0, SA.L 0.0 12.0, SA.L 6.0 6.0, SA.Z ] ] ]

placeLinePoint :: Vec2D -> Vec2D -> Vec2D
placeLinePoint p t = { x: p.x + px, y: p.y + py }
  where
    px  = cos phi * placeRadius
    py  = sin phi * placeRadius
    dx  = t.x - p.x
    dy  = t.y - p.y
    phi = atan2 dy dx

transitionLinePoint :: Vec2D -> Vec2D -> Vec2D
transitionLinePoint p t = { x: t.x - tx, y: t.y - ty }
  where
    tx = dx / u * placeRadius
    ty = dy / u * placeRadius
    dx = t.x - p.x
    dy = t.y - p.y
    u  = max (abs dx) (abs dy)

--------------------------------------------------------------------------------

arrowheadMarkerId :: String
arrowheadMarkerId = "arrowhead"

sqr :: Number -> Number
sqr x = x * x
