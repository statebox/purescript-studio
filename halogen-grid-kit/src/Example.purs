module Example where

import Prelude hiding (div)

import Data.Array ((..))
import Data.Int (toNumber, floor)
import Data.Lens (Lens, (+~), (-~), (%~))
import Data.Lens.Record (prop)
import Data.Maybe
import Data.Number (fromString)
import Data.Symbol (SProxy(..))
import Data.Vec3 (Vec2, vec2, point2, _x, _y, Box(..), boxSize, boxCenter)
import Data.Vec3.AffineTransform
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML hiding (prop)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties hiding (min, max, prop)
import Halogen.HTML.Properties as H
import Math (pow, pi)
import Svg.Elements as S
import Svg.Attributes as S

import GridKit.KeyHandler
import View.ReactiveInput as RI
import View.GridKit.Grid as Grid
import View.GridKit.Point as Point


type Input = {}

data Action
  = ChangeState (State -> State)

type State =
  { logSpacing :: Number
  , logScale :: Number
  , posX :: Number
  , posY :: Number
  , radius :: Number
  , count :: Number
  , keyHelpVisible :: Boolean
  }

_logScale :: ∀ a b r. Lens { logScale :: a | r } { logScale :: b | r } a b
_logScale = prop (SProxy :: SProxy "logScale")

_keyHelpVisible :: ∀ a b r. Lens { keyHelpVisible :: a | r } { keyHelpVisible :: b | r } a b
_keyHelpVisible = prop (SProxy :: SProxy "keyHelpVisible")

type ChildSlots =
  ( grid :: Grid.Slot Unit
  , point :: Point.Slot Int
  )

grid :: ∀ m. MonadEffect m => Grid.Input -> H.ComponentHTML Action ChildSlots m
grid input = slot (SProxy :: SProxy "grid") unit Grid.ui input (const Nothing)

point :: ∀ m. MonadEffect m => Int -> Point.Input -> H.ComponentHTML Action ChildSlots m
point id input = slot (SProxy :: SProxy "point") id Point.ui input (const Nothing)

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui =
  RI.mkComponent
    { initialState:
      { logSpacing: 1.0
      , logScale: 0.0
      , posX: 0.0
      , posY: 0.0
      , radius: 0.5
      , count: 10.0
      , keyHelpVisible: true
      }
    , render
    , handleAction
    , handleInput: \_ -> pure unit
    }

render :: ∀ m. MonadEffect m => Input -> State -> H.ComponentHTML Action ChildSlots m
render _ { logSpacing, logScale, posX, posY, radius, count, keyHelpVisible } = div
  [ tabIndex 0, keys.onKeyDown ]
  [ S.svg [ S.width (_x size), S.height (_y size) ] $
    [ grid gridInput ] <>
    ((1 .. floor count) <#> \n ->
      point n { position: rotate (toNumber n * 2.0 * pi / count) `transform` point2 radius 0.0, model2svg })
  , p_
    [ input [ type_ InputRange, H.min 0.0, H.max 2.0, step Any, value (show logSpacing)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logSpacing = v }) ]
    , text " Grid spacing"
    ]
  , p_
    [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show logScale)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logScale = v }) ]
    , text " Scale"
    ]
  , p_
    [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show posX)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posX = v }) ]
    , text " X"
    ]
  , p_
    [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show posY)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posY = v }) ]
    , text " Y"
    ]
  , p_
    [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show radius)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { radius = v }) ]
    , text " Radius"
    ]
  , p_
    [ input [ type_ InputRange, H.min 1.0, H.max 100.0, step (Step 1.0), value (show count)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { count = v }) ]
    , text " Count"
    ]
  , keys.helpPopup keyHelpVisible
  ]
    where
      scaling = pow 10.0 logScale
      pos = vec2 posX posY
      range = Box
        { topLeft: (vec2 (-0.5) (-0.5) - pos) / pure scaling
        , bottomRight: (vec2 0.5 0.5 - pos) / pure scaling
        }
      size = vec2 777.0 600.0

      model2svg = range `containedIn` size

      gridInput =
        { gridSpacing: pow 10.0 logSpacing
        , model2svg
        , size
        }

      zoomInKey = keyHandler
        [ Shortcut metaKey "Equal" "+", Shortcut ctrlKey "Equal" "+"]
        (text "Zoom in")
        (ChangeState $ _logScale +~ 0.1)
      zoomOutKey = keyHandler
        [ Shortcut metaKey "Minus" "-", Shortcut ctrlKey "Minus" "-"]
        (text "Zoom out")
        (ChangeState $ _logScale -~ 0.1)
      keys = keysWithHelpPopup
        { keys: zoomInKey <> zoomOutKey <> debugKeyCodes
        , popupAction: ChangeState $ _keyHelpVisible %~ not
        }


containedIn :: Box Number -> Vec2 Number -> AffineTransform Number
containedIn range size = translate svgCenter * scale scaleMin * translate (-rangeCenter)
  where
    scaleXY = size / boxSize range
    scaleMin = min (_x scaleXY) (_y scaleXY)
    rangeCenter = boxCenter range
    svgCenter = size / pure 2.0


handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Void m Unit
handleAction (ChangeState f) = H.modify_ f
