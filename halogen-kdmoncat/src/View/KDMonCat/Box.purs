module View.KDMonCat.Box where

import Prelude

import Data.Bifunctor (bimap)
import Data.Maybe
import Data.Vec3 (Vec2, _x, _y)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map)
import Halogen.HTML.Properties (ref)
import Halogen.Query.Input (RefLabel(..))
import Svg.Elements as S
import Svg.Attributes hiding (path) as S
import Web.DOM.Element (Element, setAttribute)

import View.ReactiveInput as ReactiveInput
import KDMonCat.Common (VoidF)

type Input a c m =
  { content   :: H.ComponentHTML a c m
  , center    :: Vec2 Number
  , minWidth  :: Number
  , maxWidth  :: Number
  , minHeight :: Number
  , maxHeight :: Number
  , padding   :: Number
  , className :: String
  }

type State = { box :: Maybe Element }

data Action a = ContentAction a

-- the output is the type of actions in the content
type Slot contentAction = H.Slot VoidF contentAction

boxView :: ∀ q a c m. MonadEffect m => H.Component HTML q (Input a c m) a m
boxView = ReactiveInput.mkComponent { initialState, render, handleInput, handleAction }

initialState :: State
initialState = { box: Nothing }

boxRef :: RefLabel
boxRef = RefLabel "box"

contentRef :: RefLabel
contentRef = RefLabel "content"

parentRef :: RefLabel
parentRef = RefLabel "parent"

render :: ∀ a c m. Input a c m -> State -> H.ComponentHTML (Action a) c m
render { content, className } _ = S.g []
  [ S.rect [ ref boxRef, S.class_ className ]
  , S.g [ ref contentRef ]
        [ mapAction ContentAction content ]
  ]

handleInput :: ∀ a c m. MonadEffect m => Input a c m -> H.HalogenM State (Action a) c a m Unit
handleInput { center, minWidth, maxWidth, minHeight, maxHeight, padding } = do
  mbox     <- H.getRef boxRef
  mcontent <- H.getRef contentRef

  case mbox, mcontent of

    -- make the box content fit
    Just box, Just content -> do
      liftEffect do
        setAttribute "style" "transform: scale(1.0); transform-origin: 0px 0px" content

      svgrect <- content # getBBox # liftEffect

      let
        midX   = svgrect.x + svgrect.width  / 2.0
        midY   = svgrect.y + svgrect.height / 2.0
        scale  = min 1.0 (min (maxWidth / svgrect.width) (maxHeight / svgrect.height))
        width  = max minWidth  (svgrect.width  * scale) + padding * 2.0
        height = max minHeight (svgrect.height * scale) + padding * 2.0
        x      = _x center - width  / 2.0
        y      = _y center - height / 2.0
        dx     = _x center - midX
        dy     = _y center - midY

      liftEffect do
        setAttribute "x"      (show x)      box
        setAttribute "y"      (show y)      box
        setAttribute "width"  (show width)  box
        setAttribute "height" (show height) box

      liftEffect do
        setAttribute "style" (
          "transform: translate(" <> show dx <> "px, " <> show dy <> "px) scale(" <> show scale <> ");" <>
          "transform-origin: " <> show midX <> "px " <> show midY <> "px") content

    _, _ -> pure unit

handleAction :: ∀ a c m. MonadEffect m => Action a -> H.HalogenM State (Action a) c a m Unit
handleAction (ContentAction a) = H.raise a

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
