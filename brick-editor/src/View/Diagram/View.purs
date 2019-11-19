module View.Diagram.View where

import Prelude
import Data.Array (snoc)
import Data.Foldable (elem)
import Data.Int (toNumber, fromStringAs, hexadecimal)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Ord (abs)
import Data.Vec3 (Vec3, Vec2, _x, _y, _z, vec3)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes (CSSLength(..))
import Svg.Attributes as SA
import Svg.Elements (rect)
import Svg.Elements as SE
import Svg.Util (domToSvgCoordinates)
import View.Diagram.Common (snap)
import View.Diagram.Model
import View.Diagram.Update (MouseMsg(..))
import Web.HTML (HTMLElement)
import Web.UIEvent.MouseEvent (clientX, clientY)

-- TODO: if there is no HTMLElement it does not make sense to draw anything
diagramEditorSVG :: ∀ a. Maybe HTMLElement -> Model -> HTML a MouseMsg
diagramEditorSVG maybeElement model =
  SE.svg [ SA.viewBox sceneLeft sceneTop w h
         , HP.ref componentRefLabel
         , HE.onMouseMove $ \e -> Just $ MousePos  (svg e maybeElement)
         , HE.onMouseDown $ \e -> Just $ MouseDown (svg e maybeElement)
         , HE.onMouseUp   $ \e -> Just $ MouseUp   (svg e maybeElement)
         ]
         (ghosts <> operators `snoc` cursor)
  where
    operators = operator (_ `elem` model.selectedOpId) s <$> model.ops
    ghosts    = operatorGhosts s model
    cursor    = renderCursor s model.cursorPos
    w         = toNumber model.config.width
    h         = toNumber model.config.height
    s         = model.config.scale
    svg e     = maybe zero (\el -> xy2Vec3 (domToSvgCoordinates el { x: clientX e, y: clientY e }))
    xy2Vec3 { x, y } = vec3 x y 0

    -- TODO ???
    sceneLeft = zero
    sceneTop  = zero

componentRefLabel :: H.RefLabel
componentRefLabel = H.RefLabel "diagram-editor-ref-label"

--------------------------------------------------------------------------------

renderCursor :: ∀ a. Int -> Vec2 Int -> HTML a MouseMsg
renderCursor s cxy =
  SE.path [ SA.stroke $ Just color, SA.fill $ Just colorA, SA.d pts ]
  where
    cx = _x cxy
    cy = _y cxy
    p x u = toNumber $ (s / 2) * x + s * u
    pts = [ SA.Abs $ SA.M (p 0 cx) (p 0 cy)
          , SA.Abs $ SA.L (p 1 cx) (p 1 cy)
          , SA.Abs $ SA.L (p 0 cx) (p 2 cy)
          , SA.Abs SA.Z
          ]
    color  = SA.RGB  90 99 120
    colorA = SA.RGBA 90 99 120 128.0

operator :: ∀ a. (OperatorId -> Boolean) -> Int -> Operator -> HTML a MouseMsg
operator isSelected s o =
  SE.g [ SA.class_ $ "css-operator-container" <> guard (isSelected o.identifier) " css-selected" ]
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
    xyw = (s*_) <$> o.pos
    x   = _x xyw
    y   = _y xyw
    w   = _z xyw
    h   = s
    pad = 9
    pp  = pad + pad

operatorSegment :: ∀ a. Operator -> OperatorHandle -> Int -> Int -> Int -> Int -> HTML a MouseMsg
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
operatorGhosts :: ∀ a. Int -> Model -> Array (HTML a MouseMsg)
operatorGhosts s model =
  let
    ddModel = dragDelta model
  in
    case model.dragStart of
      DragStartedOnBackground xy ->
        let
          xyw = vec3 identity identity (pure <<< abs <<< _x $ ddModel) <*> xy
        in
          [ operatorGhostSnapped s xyw (abs $ _y ddModel) ]
      DragStartedOnOperator _ op handle ->
        let
          xyw  = ((s*_) <$> op.pos) - ddModel
          w    = _z xyw
          axyw = if w > zero then zero else vec3 w zero (-2 * w)
          h    = s
        in
          [ operatorGhostSnapped s (xyw + axyw) h
          , operatorGhost        s (xyw + axyw) h
          ]
      _ -> []

operatorGhost :: ∀ a. Int -> Vec3 Int -> Int -> HTML a MouseMsg
operatorGhost s xyw h =
  rect [ SA.class_ "css-ghost"
       , SA.width  $ toNumber (_z xyw)
       , SA.height $ toNumber h
       , SA.x      $ toNumber (_x xyw)
       , SA.y      $ toNumber (_y xyw)
       ]

operatorGhostSnapped :: ∀ a. Int -> Vec3 Int -> Int -> HTML a MouseMsg
operatorGhostSnapped s xyw h =
  rect [ SA.class_ "css-ghost-snapped"
       , SA.width  $ toNumber $ snap s (_z xyw)
       , SA.height $ toNumber $ h
       , SA.x      $ toNumber $ snap s (_x xyw)
       , SA.y      $ toNumber $ snap s (_y xyw)
       ]
