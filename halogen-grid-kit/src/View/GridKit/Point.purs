module View.GridKit.Point where

import Prelude

import Data.Vec3 (Vec2, _x, _y)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML
import Svg.Elements as S
import Svg.Attributes hiding (path) as S

import View.ReactiveInput as RI

type Input =
  { position :: Vec2 Number
  , pixelSize :: Number
  }

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui =
  RI.mkComponent
    { initialState: {}
    , render
    , handleAction: \_ -> pure unit
    , handleInput: \_ -> pure unit
    }

render :: ∀ m. Input -> {} -> H.ComponentHTML Void () m
render { position, pixelSize } _ = do
  S.circle [ S.attr (AttrName "class") "point", S.cx (_x position), S.cy (_y position), S.r pixelSize ]
