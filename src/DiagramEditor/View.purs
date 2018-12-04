module DiagramEditor.View where

import Prelude
import Data.Int (toNumber)
import Data.Maybe
import Data.Ord (abs)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Halogen.HTML (HTML, div, pre)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseOver, onMouseOut)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Attributes (Color(RGB, RGBA), FontSize(..), CSSLength(..))
import Svg.Elements as SE
import Svg.Elements (rect)
import Web.UIEvent.MouseEvent (clientX, clientY)

import DiagramEditor.Common
import DiagramEditor.Update
import DiagramEditor.Model
import View.Common (styleStr)

-- TODO eliminate?
type Svg a = HTML Void a

diagramEditorSVG :: Model -> Svg MouseMsg
diagramEditorSVG model =
  SE.svg [ SA.viewBox sceneLeft sceneTop w h
         , HP.ref componentRefLabel
         , HE.onMouseMove $ \e -> Just $ MousePosition (clientX e) (clientY e)
         , HE.onMouseDown $ \e -> Just $ MouseDown     (clientX e) (clientY e)
         , HE.onMouseUp   $ \e -> Just $ MouseUp       (clientX e) (clientY e)
         ]
         (ghosts <> operators)
  where
    operators = operator s <$> model.ops
    ghosts    = operatorGhosts s model
    w         = toNumber model.config.width
    h         = toNumber model.config.height
    s         = model.config.scale

    -- TODO ???
    sceneLeft = 0.0
    sceneTop  = 0.0

componentRefLabel :: H.RefLabel
componentRefLabel = H.RefLabel "diagram-editor-ref-label"

--------------------------------------------------------------------------------

operator :: Int -> Operator -> Svg MouseMsg
operator s o =
  SE.g [ SA.class_ "css-operator-container" ]
       [ operatorSegment o OpCenter x             y w   h
       , operatorSegment o OpLeft   x             y pad h
       , operatorSegment o OpRight  (x + w - pad) y pad h
       , SE.text [ SA.class_ "css-operator-label"
                 , SA.x         $ toNumber $ x + w / 2
                 , SA.y         $ toNumber $ y + h / 2
                 , SA.font_size $ SA.FontSizeLength (Px (toNumber h/2.0))
                 ]
                 [ HH.text o.label ]
       ]
  where
    x   = s * o.x
    y   = s * o.y
    w   = s * o.w
    h   = s
    pad = 9
    pp  = pad + pad

operatorSegment :: Operator -> OperatorHandle -> Int -> Int -> Int -> Int -> Svg MouseMsg
operatorSegment op region x y w h =
  SE.rect $
    [ SA.class_      $ case region of OpCenter -> "css-operator-center"
                                      OpLeft   -> "css-operator-left"
                                      OpRight  -> "css-operator-right"
    , SA.width       $ toNumber w
    , SA.height      $ toNumber h
    , SA.x           $ toNumber x
    , SA.y           $ toNumber y
    , HE.onMouseOver $ \mouseEvent -> Just $ MouseIsOver op region
    , HE.onMouseOut  $ \mouseEvent -> Just $ MouseIsOut  op
    ]


-- ghosts ----------------------------------------------------------------------

-- TODO parameter s is redundant, equals model.config.scale
operatorGhosts :: Int -> Model -> Array (Svg MouseMsg)
operatorGhosts s model = let (dx /\ dy /\ dw) = dragDelta model in case model.dragStart of
  DragStartedOnBackground (x /\ y) ->
    [ operatorGhostSnapped s x y (abs dx) (abs dy) ]
  DragStartedOnOperator _ op handle ->
    let
      x /\ y   = (s * op.x - dx) /\ (s * op.y - dy)
      w        = (s * op.w - dw)
      ax /\ aw = if w > 0 then 0 /\ 0 else w /\ (-2 * w)
      h        = s
    in
      [ operatorGhostSnapped s (x+ax) y (w+aw) h
      , operatorGhost        s (x+ax) y (w+aw) h
      ]
  _ -> []

operatorGhost :: Int -> Int -> Int -> Int -> Int -> Svg MouseMsg
operatorGhost s x y w h =
  rect [ SA.class_ "css-ghost"
       , SA.width  $ toNumber w
       , SA.height $ toNumber h
       , SA.x      $ toNumber x
       , SA.y      $ toNumber y
       ]

operatorGhostSnapped :: Int -> Int -> Int -> Int -> Int -> Svg MouseMsg
operatorGhostSnapped s x y w h =
  rect [ SA.class_ "css-ghost-snapped"
       , SA.width  $ toNumber $ snap s w
       , SA.height $ toNumber $ h
       , SA.x      $ toNumber $ snap s x
       , SA.y      $ toNumber $ snap s y
       ]
