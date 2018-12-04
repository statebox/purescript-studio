module DiagramEditor.Inspector where

import Prelude hiding (div)

import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))

import Halogen as H
import Halogen (ComponentDSL)
import Halogen.HTML as HH
import Halogen.HTML (HTML, div, br)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)

import DiagramEditor.Model
import DiagramEditor.Update (Query, State)
import DiagramEditor.Common (snap, showVec2, showVec3)

view :: State -> HTML Void (Query Unit)
view state@{model} =
  HH.pre [ classes [ ClassName "css-diagram-editor-properties-view" ] ]
         [ prop "ops"      $ show $ map (_.identifier) model.ops
         , prop "over"     $ show model.mouseOver
         , prop "pos"      $ showVec2 model.mousePosition
         , prop "pressed"  $ show model.mousePressed
         , prop "dragging" $ show model.dragStart
         , prop "dx,dy,dw" $ showVec3 (dx /\ dy /\ dw)
         , prop "sdx,sdy"  $ showVec2 (sdx /\ sdy)
         , prop "mdx,mdy"  $ showVec2 (mdx /\ mdy)
         , prop "valid"    $ show $ isValidDrag model
         , code "-----------------------\n"
         , prop "bounds"   $ maybe "Nothing" (\r -> "Just " <> viewRect r) state.boundingClientRectMaybe
         ]
    where
      prop :: String -> String -> HTML Void (Query Unit)
      prop k v = code $ k <> " = " <> v <> "\n"

      code str = HH.code [] [ HH.text str ]

      dx /\ dy /\ dw = dragDelta model
      sdx /\ sdy     = (snap model.config.scale dx) /\ (snap model.config.scale dy)
      mdx /\ mdy     = (sdx / model.config.scale)   /\ (sdy / model.config.scale)

viewRect :: forall r. { top :: Number, left :: Number | r } -> String
viewRect { top, left } = showVec2 (top /\ left)
