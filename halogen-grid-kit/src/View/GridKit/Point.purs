module View.GridKit.Point where

import Prelude

import Data.Vec3 (Point2, _x, _y)
import Data.Vec3.AffineTransform
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML
import Svg.Elements as S
import Svg.Attributes hiding (path) as S

import View.ReactiveInput as ReactiveInput

type Input =
  { position :: Point2 Number
  , model2svg :: AffineTransform Number
  }

data VoidF a
type Slot = H.Slot VoidF Void

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui = ReactiveInput.mkComponent
  { initialState: {}
  , render
  , handleAction: \_ -> pure unit
  , handleInput: \_ -> pure unit
  }

render :: ∀ m. Input -> {} -> H.ComponentHTML Void () m
render { position, model2svg } _ =
  S.circle [ S.class_ "point", S.cx (_x center), S.cy (_y center), S.r 5.0 ]
  where
    center = model2svg `transform` position
