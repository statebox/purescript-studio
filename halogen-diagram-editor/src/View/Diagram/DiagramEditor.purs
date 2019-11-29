module View.Diagram.DiagramEditor where

import Prelude hiding (div)

import Data.Array (snoc, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (Vec3, vec3, _x, _y, _z)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML (HTML, div)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, tabIndex)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (HTMLElement)
import Web.UIEvent.KeyboardEvent (code, toEvent)

import View.Diagram.Common (classesWithNames)
import View.Diagram.Model (DragStart(..), Operators)
import View.Diagram.Update (Action(..), MouseMsg(..), Msg(..), State, evalModel)
import View.Diagram.View as View
import View.Diagram.Inspector as Inspector

initialState :: Operators -> State
initialState ops =
  { model:
    { config:        { scale: 24, width: 550, height: 450 }
    , ops:           ops
    , selectedOpId:  Nothing
    , mouseOver:     Nothing
    , mousePos:      vec3 0 0 0
    , cursorPos:     vec3 0 0 0
    , mousePressed:  false
    , dragStart:     DragNotStarted
    }
  , msg:                ""
  , componentElemMaybe: Nothing
  }

ui :: ∀ m q. MonadAff m => H.Component HTML q Operators Msg m
ui = H.mkComponent { initialState, render, eval: mkEval $ defaultEval {
    handleAction = handleAction, receive = Just <<< UpdateDiagram, initialize = Just Initialize
  }}
  where
    render :: State -> ComponentHTML Action () m
    render state =
      div [ classes [ ClassName "css-diagram-editor" ]
          , tabIndex 0
          , HE.onKeyDown $ Just <<< KeyboardAction
          ]
          [ div [ classes [] ]
                [ View.diagramEditorSVG state.componentElemMaybe state.model <#> MouseAction
                , div [ classesWithNames [ "mt-4", "rb-2", "p-4", "bg-grey-lightest", "text-grey-dark", "rounded", "text-sm" ] ]
                      [ Inspector.view state ]
                ]
          ]

    handleAction :: Action -> HalogenM State Action () Msg m Unit
    handleAction = case _ of

      CreateOp -> do
        m <- H.get <#> _.model
        let ops = m.ops
        let id = length ops
        let newOp = { identifier: "new" <> show id, pos: m.cursorPos + vec3 1 0 7, label: "new" <> show id }
        H.modify_ \st -> st { model = m { cursorPos = m.cursorPos + vec3 0 1 0 } }
        H.raise $ OpsChanged (ops `snoc` newOp)

      MoveCursor delta -> do
        m <- H.get <#> _.model
        let { cursorPos, config: { scale, width, height } } = m
        let cursorPos' = clamp2d (width/scale+1) (height/scale+1) (cursorPos + delta)
        H.modify_ \st -> st { model = m { cursorPos = cursorPos' } }

      KeyboardAction k -> do
        H.liftEffect $ preventDefault $ toEvent k
        case code k of
          "ArrowLeft"  -> move (-1)  0
          "ArrowUp"    -> move   0 (-1)
          "ArrowRight" -> move   1   0
          "ArrowDown"  -> move   0   1
          "Space"      -> handleAction CreateOp
          _            -> pure unit
        where
          move dx dy = handleAction $ MoveCursor (vec3 dx dy zero)

      MouseAction msg -> do
        state <- H.get
        let (opsMoved /\ model') = evalModel msg state.model
            state' = state { model = model' }
        if opsMoved
          then H.raise $ OpsChanged model'.ops
          else pure unit
        let isOperatorClicked = case msg of
              MouseUp _ -> true
              _         -> false

            clickedOperatorId = case state'.model.mouseOver of
              Just (op /\ oph) | isOperatorClicked -> Just op.identifier
              _                                    -> Nothing

            state'' = if isOperatorClicked then state' { model = state'.model { selectedOpId = clickedOperatorId } }
                                           else state'

        H.put state''

        maybe (pure unit) (H.raise <<< OperatorClicked) clickedOperatorId

      UpdateDiagram ops -> do
        H.modify_ \state -> state { model = state.model { ops = ops } }

      Initialize -> do
        componentElemMaybe <- getHTMLElementRef' View.componentRefLabel
        H.modify_ \state -> state { componentElemMaybe = componentElemMaybe }

--------------------------------------------------------------------------------

clamp2d :: Int -> Int -> Vec3 Int -> Vec3 Int
clamp2d width height xy = vec3 (clamp 0 (width - 1) (_x xy)) (clamp 0 (height - 1) (_y xy)) (_z xy)

-- TODO this is generally useful; move elsewhere
-- This was made because the original implementation from Halogen.Query doesn't seem to work, at least in this case:
--      getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
getHTMLElementRef' :: forall s a i o m. H.RefLabel -> HalogenM s a i o m (Maybe HTMLElement)
getHTMLElementRef' = map (map elementToHTMLElement) <<< H.getRef
  where
    elementToHTMLElement :: Element -> HTMLElement
    elementToHTMLElement = unsafeCoerce
