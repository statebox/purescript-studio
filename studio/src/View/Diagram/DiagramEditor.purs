module View.Diagram.DiagramEditor where

import Prelude hiding (div)
import Debug.Trace (trace)

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (vec2, _x, _y)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML (HTML, div)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, tabIndex)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.HTML.HTMLElement (HTMLElement)
import Web.UIEvent.KeyboardEvent (code)

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
    , mousePos:      vec2 0 0
    , cursorPos:     vec2 0 0
    , mousePressed:  false
    , dragStart:     DragNotStarted
    }
  , msg:                ""
  , componentElemMaybe: Nothing
  }

clamp2d :: Int -> Int -> Int /\ Int -> Int /\ Int
clamp2d width height (x /\ y) = clamp 0 (width - 1) x /\ clamp 0 (height - 1) y

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
                , div [ classes [ ClassName "mt-4"
                                , ClassName "rb-2"
                                , ClassName "p-4"
                                , ClassName "bg-grey-lightest"
                                , ClassName "text-grey-dark"
                                , ClassName "rounded"
                                , ClassName "text-sm"
                                ] ]
                      [ Inspector.view state ]
                ]
          ]

    handleAction :: Action -> HalogenM State Action () Msg m Unit
    handleAction = case _ of

      MoveCursor (dx/\dy) -> do
        st <- H.get
        let m = st.model
        let {scale,width,height} = m.config
        let (x'/\y') = clamp2d (width/scale+1) (height/scale+1) ((_x m.cursorPos + dx) /\ (_y m.cursorPos + dy))
        H.modify_ \st -> st { model = m { cursorPos = vec2 x' y'} }
        H.raise CursorMoved

      KeyboardAction k ->
        let act dx dy = handleAction $ MoveCursor (dx /\ dy) in
        case code k of
          "ArrowLeft"  -> act (-1)  0
          "ArrowUp"    -> act   0 (-1)
          "ArrowRight" -> act   1   0
          "ArrowDown"  -> act   0   1
          -- TODO space
          _            -> pure unit

      MouseAction msg -> do
        state <- H.get
        let state' = state { model = evalModel msg state.model }

            isOperatorClicked = case msg of
              MouseUp _ -> true
              _         -> false

            clickedOperatorId = case state'.model.mouseOver of
              Just (op /\ oph) | isOperatorClicked -> Just op.identifier
              _                                    -> Nothing

            state'' = if isOperatorClicked then state' { model = state'.model { selectedOpId = clickedOperatorId } }
                                           else state'

        H.put state''

        maybe (pure unit) (H.raise <<< OperatorClicked) clickedOperatorId

      UpdateDiagram ops ->
        H.modify_ \state -> state { model = state.model { ops = ops } }

      Initialize -> do
        componentElemMaybe <- getHTMLElementRef' View.componentRefLabel
        H.modify_ \state -> state { componentElemMaybe = componentElemMaybe }

-- TODO this is generally useful; move elsewhere
-- This was made because the original implementation from Halogen.Query doesn't seem to work, at least in this case:
--      getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
getHTMLElementRef' :: forall s a i o m. H.RefLabel -> HalogenM s a i o m (Maybe HTMLElement)
getHTMLElementRef' = map (map elementToHTMLElement) <<< H.getRef
  where
    elementToHTMLElement :: Element -> HTMLElement
    elementToHTMLElement = unsafeCoerce
