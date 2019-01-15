module View.Petrinet.Arrow where

import Prelude (max, negate, ($), (*), (+), (-), (/), (<$>), (>), (>=), (<>), pure)
import Data.Vec2D (Vec2D, vec2, _x, _y)
import Data.Vec2D as Vec2
import Halogen.HTML (HTML)
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

import View.Petrinet.Config

-- | This refers to an `arrowheadMarkerId`, for which a marker must be defined in the `<defs>`.
-- | Such a marker is defined provided here as `svgArrowheadMarker`.
svgArrow :: forall p i. Vec2D -> Vec2D -> Boolean -> HTML p i
svgArrow src dest isPost =
  case isPost of
    true  -> let src'  = transitionLinePoint dest src
                 dest' = placeLinePoint      dest src
             in  svgArrowLine src' dest'
    false -> let src'  = placeLinePoint      src dest
                 dest' = transitionLinePoint src dest
             in  svgArrowLine src' dest'

svgArrowLine :: forall p i. Vec2D -> Vec2D -> HTML p i
svgArrowLine src dest =
  SE.line
    [ SA.class_ "css-arrow"
    , SA.x1 $ _x src
    , SA.y1 $ _y src
    , SA.x2 $ _x dest
    , SA.y2 $ _y dest
    , SA.markerEnd $ "url(#" <> arrowheadMarkerId <> ")"
    ]


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
placeLinePoint p t = p + p'
  where
    p'  = vec2 (cos phi) (sin phi) * pure placeRadius
    d   = t - p
    phi = atan2 (_y d) (_x d)

transitionLinePoint :: Vec2D -> Vec2D -> Vec2D
transitionLinePoint p t = t - t'
  where
    t' = d / pure (u * placeRadius)
    d = t - p
    u  = abs (_x d) `max` abs (_y d)

--------------------------------------------------------------------------------

arrowheadMarkerId :: String
arrowheadMarkerId = "arrowhead"

sqr :: Number -> Number
sqr x = x * x
