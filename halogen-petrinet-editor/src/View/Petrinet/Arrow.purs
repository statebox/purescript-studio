module View.Petrinet.Arrow where

import Prelude (max, negate, ($), (*), (+), (-), (/), (<$>), (>), (>=), (<>), pure)
import Data.Vec3 (Vec2D, vec2, _x, _y)
import Data.Vec3 as Vec2
import Halogen.HTML (HTML)
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)
import Data.Array (head, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)

import View.Petrinet.Config

-- | This refers to an `arrowheadMarkerId`, for which a marker must be defined in the `<defs>`.
-- | Such a marker is defined provided here as `svgArrowheadMarker`.
svgArrow :: forall p i. Vec2D -> Vec2D -> Array Vec2D -> Boolean -> HTML p i
svgArrow src dest waypoints isPost =
  case isPost of
    true  -> let src'  = transitionLinePoint toSrc  src
                 dest' = placeLinePoint      dest   toDest
             in  svgArrowLine src' dest' waypoints
    false -> let src'  = placeLinePoint      src    toSrc
                 dest' = transitionLinePoint toDest dest
             in  svgArrowLine src' dest' waypoints
  where
    toSrc = fromMaybe dest (head waypoints)
    toDest = fromMaybe src (last waypoints)

svgArrowLine :: forall p i. Vec2D -> Vec2D -> Array Vec2D -> HTML p i
svgArrowLine src dest waypoints =
  SE.path
    [ SA.class_ "css-arrow"
    , SA.d $ SA.Abs <$> [ SA.M (_x src) (_y src) ] <> ((\p -> SA.L (_x p) (_y p)) <$> waypoints) <> [ SA.L (_x dest) (_y dest) ]
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
    t' = d * pure (placeRadius / u)
    d = t - p
    u  = abs (_x d) `max` abs (_y d)

--------------------------------------------------------------------------------

arrowheadMarkerId :: String
arrowheadMarkerId = "arrowhead"

sqr :: Number -> Number
sqr x = x * x
