module View.KDMonCat.Box where

import Prelude

import Data.Bifunctor (bimap)
import Data.Maybe
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map)
import Halogen.HTML.Properties (ref)
import Halogen.Query.Input (RefLabel(..))
import Svg.Elements as S
import Svg.Attributes hiding (path) as S
import Web.DOM.Element (Element, setAttribute)

import KDMonCat.Common (VoidF)

import Debug.Trace

type Input a c m =
  { content   :: H.ComponentHTML a c m
  , minWidth  :: Number
  , maxWidth  :: Number
  , minHeight :: Number
  , maxHeight :: Number
  , padding   :: Number
  , className :: String
  }

type State a c m =
  { input :: Input a c m
  , box   :: Maybe Element
  }

data Action a c m
  = Initialize
  | Update (Input a c m)
  | ContentAction a

-- the output is the type of actions in the content
type Slot contentAction = H.Slot VoidF contentAction

boxView :: ∀ q a c m. MonadEffect m => H.Component HTML q (Input a c m) a m
boxView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Update, initialize = Just Initialize }
    }

initialState :: ∀ a c m. Input a c m -> State a c m
initialState = { input: _, box: Nothing }

boxRef :: RefLabel
boxRef = RefLabel "box"

contentRef :: RefLabel
contentRef = RefLabel "content"

parentRef :: RefLabel
parentRef = RefLabel "parent"

render :: ∀ a c m. State a c m -> H.ComponentHTML (Action a c m) c m
render { input: { content, className } } = S.g []
  [ S.rect [ ref boxRef, S.attr (AttrName "class") className ]
  , S.g [ ref contentRef, S.transform [S.Scale 1.0 1.0] ]
        [ mapAction ContentAction content ]
  ]

handleAction :: ∀ a c m. MonadEffect m => Action a c m -> H.HalogenM (State a c m) (Action a c m) c a m Unit
handleAction = case _ of

  Initialize -> do
    mbox     <- H.getRef boxRef
    mcontent <- H.getRef contentRef
    { input: { minWidth, maxWidth, minHeight, maxHeight, padding } } <- H.get

    case mbox, mcontent of

      -- make the box content fit
      Just box, Just content -> do
        svgrect <- content # getBBox # liftEffect

        let
          midX   = svgrect.x + svgrect.width  / 2.0
          midY   = svgrect.y + svgrect.height / 2.0
          scale  = min 1.0 (min (maxWidth / svgrect.width) (maxHeight / svgrect.height))
          width  = max minWidth  (svgrect.width  * scale) + padding * 2.0
          height = max minHeight (svgrect.height * scale) + padding * 2.0
          x      = midX - width  / 2.0
          y      = midY - height / 2.0

        liftEffect do
          setAttribute "x"      (show x)      box
          setAttribute "y"      (show y)      box
          setAttribute "width"  (show width)  box
          setAttribute "height" (show height) box

        liftEffect do
          setAttribute "transform"        ("scale(" <> show scale <> ")") content
          setAttribute "transform-origin" (show midX <> " " <> show midY) content

      _, _ -> pure unit

  Update input -> do
    H.modify_ _ { input = input }
    handleAction Initialize

  ContentAction a ->
    H.raise a

mapAction :: ∀ m c a b. (a -> b) -> H.ComponentHTML a c m -> H.ComponentHTML b c m
mapAction f = bimap (map f) f

--------------------------------------------------------------------------------

-- TODO move to SVG lib
type SVGRect =
  { x      :: Number
  , y      :: Number
  , width  :: Number
  , height :: Number
  }

-- TODO move to SVG lib
foreign import getBBox :: Element -> Effect SVGRect
