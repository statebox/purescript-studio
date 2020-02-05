module View.GridKit.Rect where

import Data.Vec3 (Point2, Vec2, _x, _y)
import Data.Vec3.AffineTransform
import Svg.Elements as S
import Svg.Attributes as S

import GridKit.UIComponent

newtype Rect = Rect
  { topLeft :: Point2 Number
  , size    :: Vec2 Number
  }

instance uiComponentRect :: UIComponent Rect where
  toSVG model2svg (Rect { topLeft, size }) =
    [ S.rect [ S.class_ "rect", S.x (_x xy), S.y (_y xy), S.width (_x wh), S.height (_y wh) ] ]
    where
      xy = model2svg `transform` topLeft
      wh = model2svg `transform` size
