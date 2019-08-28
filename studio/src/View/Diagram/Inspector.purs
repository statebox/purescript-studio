module View.Diagram.Inspector where

import Prelude hiding (div)

import Data.Vec3 (vec2)

import Halogen (ComponentHTML)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (classes)

import View.Diagram.Model (dragDelta, isValidDrag)
import View.Diagram.Update (Action, State)
import View.Diagram.Common (snap)

view :: ∀ m. State -> ComponentHTML Action () m
view state@{model} =
  HH.pre [ classes [ ClassName "css-diagram-editor-properties-view" ] ]
         [ prop "ops"         $ show $ map (_.identifier) model.ops
         , prop "over"        $ show model.mouseOver
         , prop "pos"         $ show model.mousePos
         , prop "pressed"     $ show model.mousePressed
         , prop "dragging"    $ show model.dragStart
         , prop "dx,dy,dw"    $ show dxdydw
         , prop "sdx,sdy,sdw" $ show sdxdydw
         , prop "mdx,mdy,mdw" $ show mdxdydw
         , prop "valid"       $ show $ isValidDrag model
         , prop "selectedOpId"$ show model.selectedOpId
         , code "-----------------------\n"
         ]
    where
      prop :: String -> String -> ComponentHTML Action () m
      prop k v = code $ k <> " = " <> v <> "\n"

      code str = HH.code [] [ HH.text str ]

      dxdydw   = dragDelta model
      sdxdydw  = map (snap model.config.scale) dxdydw
      mdxdydw  = map (\x -> x / model.config.scale) sdxdydw

viewRect :: forall r. { top :: Number, left :: Number | r } -> String
viewRect { top, left } = show $ vec2 top left
