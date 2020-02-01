module View.Diagram.DiagramEditor where

import Prelude hiding (div)

import Data.Array (snoc, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.Vec3 (Vec3, vec3, _x, _y, _z)
import Effect.Aff.Class (class MonadAff)
import GridKit.KeyHandler
import Halogen as H
import Halogen (ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML (HTML, div, text, button)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, tabIndex)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.HTML.HTMLElement (HTMLElement)

import View.ReactiveInput as ReactiveInput
import View.Diagram.Common (classesWithNames)
import View.Diagram.Model (DragStart(..), Operators)
import View.Diagram.Update (Action(..), MouseMsg(..), Msg(..), State, evalModel)
import View.Diagram.View as View
import View.Diagram.Inspector as Inspector

initialState :: State
initialState =
  { model:
    { config:        { scale: 24, width: 550, height: 450 }
    , selectedOpId:  Nothing
    , mouseOver:     Nothing
    , mousePos:      vec3 0 0 0
    , cursorPos:     vec3 0 0 0
    , mousePressed:  false
    , dragStart:     DragNotStarted
    }
  , msg:                ""
  , keyHelpVisible:     false
  , componentElemMaybe: Nothing
  , inspectorVisible: false
  }

ui :: ∀ m q. MonadAff m => H.Component HTML q Operators Msg m
ui = ReactiveInput.mkComponent { initialState, render, handleInput, handleAction }
  where
    keys :: KeysWithHelpPopup Action
    keys = keysWithHelpPopup
      { keys:        keyHandler [ Shortcut noMods "Space" ] (Just $ text "Insert a new operator") CreateOp <>
                     cursorKeyHandler noMods MoveCursor
      , popupAction: ToggleKeyHelp
      }

    render :: Operators -> State -> ComponentHTML Action () m
    render ops state =
      div [ classes [ ClassName "css-diagram-editor" ]
          , tabIndex 0
          , keys.onKeyDown
          ]
          [ div [ classes [] ]
                [ View.diagramEditorSVG state.componentElemMaybe ops state.model <#> MouseAction
                , div [ classes [ ClassName "css-diagram-editor-inspector-container" ] ]
                      [ div [ classes [ ClassName "css-diagram-editor-inspector-link-container" ] ]
                            [ button [ onClick \_ -> Just ToggleInspector ]
                                     [ text $ (if state.inspectorVisible then "Hide" else "Show") <> " inspector" ]
                            ]
                      , if state.inspectorVisible
                           then div [ classesWithNames [ "mt-4", "rb-2", "p-4", "bg-grey-lightest", "text-grey-dark", "rounded", "text-sm" ] ]
                                    [ Inspector.view ops state ]
                           else div [] []
                      ]
                ]
          , keys.helpPopup state.keyHelpVisible
          ]

    handleInput :: Operators -> HalogenM State Action () Msg m Unit
    handleInput _ = do
      componentElemMaybe <- getHTMLElementRef' View.componentRefLabel
      H.modify_ \state -> state { componentElemMaybe = componentElemMaybe }

    handleAction :: Operators -> Action -> HalogenM State Action () Msg m Unit
    handleAction ops = case _ of

      CreateOp -> do
        m <- H.get <#> _.model
        let id = length ops
        let newOp = { identifier: "new" <> show id, pos: m.cursorPos + vec3 1 0 7, label: "new" <> show id }
        H.modify_ \st -> st { model = m { cursorPos = m.cursorPos + vec3 0 1 0 } }
        H.raise $ OperatorsChanged (ops `snoc` newOp)

      MoveCursor delta -> do
        m <- H.get <#> _.model
        let { cursorPos, config: { scale, width, height } } = m
        let cursorPos' = clamp2d (width/scale+1) (height/scale+1) (cursorPos + delta)
        H.modify_ \st -> st { model = m { cursorPos = cursorPos' } }

      MouseAction msg -> do
        state <- H.get
        let (opsChanged /\ model') = evalModel msg ops state.model
            state' = state { model = model' }

        case opsChanged of
          Just ops' -> H.raise (OperatorsChanged ops')
          Nothing -> pure unit

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

      ToggleKeyHelp -> do
        H.modify_ $ \state -> state { keyHelpVisible = not state.keyHelpVisible }

      ToggleInspector -> do
        H.modify_ \state -> state { inspectorVisible = not state.inspectorVisible }

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
