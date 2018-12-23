module View.Petrinet.PNPRO where

import Prelude
import Data.Vec2D (Vec2(..), Box(..))
import View.Petrinet.Model (TextBox)

toTextBox :: forall r. { name :: String, x :: Number, y :: Number, width :: Number, height :: Number | r } -> TextBox
toTextBox v =
  { name: v.name
  , box: Box { topLeft: Vec2 { x: v.x, y: v.y }, bottomRight: Vec2 { x: v.x + v.width, y: v.y + v.height } }
  }
