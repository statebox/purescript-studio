module View.GridKit.Point where

import Data.Vec3 (Point2, _x, _y)
import Data.Vec3.AffineTransform
import Svg.Elements as S
import Svg.Attributes as S

import GridKit.UIComponent

newtype Point = Point (Point2 Number)

instance uiComponentPoint :: UIComponent Point where
  toSVG model2svg (Point position) =
    [ S.circle [ S.class_ "point", S.cx (_x center), S.cy (_y center), S.r 3.0 ] ]
    where
      center = model2svg `transform` position
