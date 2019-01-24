module View.Diagram.DiagramEditor where

import Prelude hiding (div)

import Data.Bifunctor (bimap)
import Data.Int (floor, round)
import Data.Maybe
import Data.Traversable (traverse)
import Data.Vec2D (Vec3, vec2, vec3)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Foreign (Foreign)
import Halogen as H
import Halogen (ComponentDSL, HalogenM)
import Halogen.HTML as HH
import Halogen.HTML (HTML, div, br)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)
import Halogen.Query (getHTMLElementRef)
import Svg.Elements as SE
import Svg.Attributes as SA
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLElement (HTMLElement, DOMRect, getBoundingClientRect)

import View.Diagram.Model
import View.Diagram.Update as Update
import View.Diagram.Update
import View.Diagram.Common
import View.Diagram.View as View
import View.Diagram.Inspector as Inspector

initialState :: Operators -> State
initialState ops =
  { model:
    { config:        { scale: 24, width: 550, height: 450 }
    , ops:           ops
    , mouseOver:     Nothing
    , mousePos:      vec2 0 0
    , mousePressed:  false
    , dragStart:     DragNotStarted
    }
  , msg: "Welcome to the Statebox string diagram editor."
  , boundingClientRectMaybe: Nothing
  }

ui :: ∀ b m. MonadAff m => H.Component HTML Query Operators b m
ui = H.component { initialState: initialState, render, eval, receiver: HE.input UpdateDiagram }
  where
    render :: State -> HTML Void (Query Unit)
    render state =
      div [ classes [ ClassName "css-diagram-editor" ] ]
          [ div [ classes [ ClassName "flex" ] ]
                [ div [ classes [ ClassName "w-5/6"] ]
                      [ (\msg -> QueryF msg unit) <$> View.diagramEditorSVG state.model ]
                , div [ classes [ ClassName "w-1/6 px-2" ] ]
                      [ Inspector.view state ]
                ]
          ]

    -- TODO We shouldn't need to getBoundingClientRect on every single model update, that is incredibly inefficient.
    --      Doing it just on initialisation and window resizing/layout changes should do.
    eval :: forall b. Query ~> ComponentDSL State Query b m
    eval = case _ of
      QueryF msg next -> do
        componentElemMaybe <- getHTMLElementRef' View.componentRefLabel
        boundingRectMaybe <- H.liftEffect $ getBoundingClientRect `traverse` componentElemMaybe
        let updater = maybe (\     state -> state { msg   = "Could not determine this component's boundingClientRect." })
                            (\rect state -> state { model = evalModel msg $ state.model })
                            boundingRectMaybe
        H.modify_ (updater <<< _ { boundingClientRectMaybe = boundingRectMaybe })
        pure next

      UpdateDiagram ops next -> do
        H.modify_ $ \state -> state { model = state.model { ops = ops } }
        pure next

-- TODO this is generally useful; move elsewhere
-- This was made because the original implementation from Halogen.Query doesn't seem to work, at least in this case:
--      getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
getHTMLElementRef' :: forall s f g p o m. H.RefLabel -> HalogenM s f g p o m (Maybe HTMLElement)
getHTMLElementRef' = map (map elementToHTMLElement) <<< H.getRef
  where
    elementToHTMLElement :: Element -> HTMLElement
    elementToHTMLElement = unsafeCoerce
