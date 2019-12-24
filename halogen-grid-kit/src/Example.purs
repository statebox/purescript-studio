module Example where

import Prelude hiding (min, max)

import Data.Maybe
import Data.Number (fromString)
import Data.Symbol (SProxy(..))
import Data.Vec3 (vec2, _x, _y, Box(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML hiding (canvas)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties
import Math (pow)


import View.ReactiveInput as RI
import View.GridKit.Canvas as Canvas
import View.GridKit.Point as Point


type Input = {}

data Action
  = ChangeState (State -> State)

type State =
  { logSpacing :: Number
  , logScale :: Number
  , posX :: Number
  , posY :: Number
  }

type ChildSlots =
  ( canvas :: Canvas.Slot Unit
  )

canvas :: ∀ m. MonadEffect m => Canvas.Input -> H.ComponentHTML Action ChildSlots m
canvas input = slot (SProxy :: SProxy "canvas") unit Canvas.ui input (const Nothing)


ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui =
  RI.mkComponent
    { initialState:
      { logSpacing: 1.0
      , logScale: 0.0
      , posX: 0.0
      , posY : 0.0
      }
    , render
    , handleAction
    , handleInput: \_ -> pure unit
    }

render :: ∀ m. MonadEffect m => Input -> State -> H.ComponentHTML Action ChildSlots m
render _ { logSpacing, logScale, posX, posY } = div_
  [ canvas canvasInput
  , p_
    [ input [ type_ InputRange, min 0.0, max 2.0, step Any, value (show logSpacing)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logSpacing = v }) ]
    , text " Grid spacing"
    ]
  , p_
    [ input [ type_ InputRange, min (-5.0), max 5.0, step Any, value (show logScale)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logScale = v }) ]
    , text " Scale"
    ]
  , p_
    [ input [ type_ InputRange, min (-5.0), max 5.0, step Any, value (show posX)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posX = v }) ]
    , text " X"
    ]
  , p_
    [ input [ type_ InputRange, min (-5.0), max 5.0, step Any, value (show posY)
            , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posY = v }) ]
    , text " Y"
    ]
  ]
    where
      scale = pow 10.0 logScale
      pos = vec2 posX posY
      canvasInput =
        { gridSpacing: pow 10.0 logSpacing
        , range: Box
          { topLeft: (vec2 (-0.5) (-0.5) - pos) * pure scale
          , bottomRight: (vec2 0.5 0.5 - pos) * pure scale
          }
        , size: vec2 777.0 600.0
        }

handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Void m Unit
handleAction (ChangeState f) = H.modify_ f
