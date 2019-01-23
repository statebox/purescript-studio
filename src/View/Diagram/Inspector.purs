module View.Diagram.Inspector where

import Prelude hiding (div)

import Data.Maybe (maybe)
import Data.Vec2D (vec2)

import Halogen.HTML as HH
import Halogen.HTML (HTML)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (classes)

import View.Diagram.Model
import View.Diagram.Update (Query, State)
import View.Diagram.Common (snap)

view :: State -> HTML Void (Query Unit)
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
         , code "-----------------------\n"
         , prop "bounds"      $ maybe "Nothing" (\r -> "Just " <> viewRect r) state.boundingClientRectMaybe
         ]
    where
      prop :: String -> String -> HTML Void (Query Unit)
      prop k v = code $ k <> " = " <> v <> "\n"

      code str = HH.code [] [ HH.text str ]

      dxdydw   = dragDelta model
      sdxdydw  = map (snap model.config.scale) dxdydw
      mdxdydw  = map (\x -> x / model.config.scale) sdxdydw

viewRect :: forall r. { top :: Number, left :: Number | r } -> String
viewRect { top, left } = show $ vec2 top left
