module View.GridKit.Rect where

import Prelude

import Data.Vec3.AffineTransform
import Data.Vec3.Box
import Data.Vec3.Vec3 (_x, _y, vectorLength, vec2, binOp)
import Svg.Elements as S
import Svg.Attributes as S

import GridKit.UIComponent

newtype Rect = Rect (Box Number)

instance uiComponentRect :: UIComponent Rect where

  toSVG model2svg (Rect box) =
    [ S.rect [ S.class_ "rect", S.x (_x xy), S.y (_y xy), S.width (_x wh), S.height (_y wh) ] ]
    where
      Box box' = mapVec3s (transform model2svg) box
      xy = box'.topLeft
      wh = boxSize (Box box')

  distance p (Rect (Box { topLeft, bottomRight })) = vectorLength v
    where
      topLeftV = topLeft - p
      bottomRightV = p - bottomRight
      v = binOp max (vec2 0.0 0.0) (binOp max topLeftV bottomRightV)
