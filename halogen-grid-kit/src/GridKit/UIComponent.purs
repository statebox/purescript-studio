module GridKit.UIComponent where

import Data.Vec3.AffineTransform
import Data.Foldable (foldMap)
import Halogen.HTML (PlainHTML)

class UIComponent a where
  toSVG :: AffineTransform Number -> a -> Array PlainHTML

instance uiComponentArray :: UIComponent a => UIComponent (Array a) where
  toSVG transform = foldMap (toSVG transform)
