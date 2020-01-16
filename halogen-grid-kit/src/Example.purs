module Example where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber, floor)
import Data.Maybe
import Data.Number (fromString)
import Data.Symbol (SProxy(..))
import Data.Vec3 (Vec2, vec2, point2, _x, _y, Box(..), boxSize, boxCenter)
import Data.Vec3.AffineTransform
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties hiding (min, max)
import Halogen.HTML.Properties as H
import Math (pow, pi)
import Svg.Elements as S
import Svg.Attributes as S

import View.ReactiveInput as ReactiveInput
import View.GridKit.Grid as Grid
import View.GridKit.Point as Point

type Input = {}

data Action = ChangeState (State -> State)

type State =
  { logSpacing :: Number
  , logScale :: Number
  , posX :: Number
  , posY :: Number
  , radius :: Number
  , count :: Number
  }

type ChildSlots =
  ( grid :: Grid.Slot Unit
  , point :: Point.Slot Int
  )

ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui = ReactiveInput.mkComponent
  { initialState:
    { logSpacing: 1.0
    , logScale: 0.0
    , posX: 0.0
    , posY: 0.0
    , radius: 0.5
    , count: 10.0
    }
  , render
  , handleAction
  , handleInput: \_ -> pure unit
  }

handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Void m Unit
handleAction (ChangeState f) = H.modify_ f

render :: ∀ m. MonadEffect m => Input -> State -> H.ComponentHTML Action ChildSlots m
render _ { logSpacing, logScale, posX, posY, radius, count } = div_
  [ S.svg [ S.width (_x size), S.height (_y size) ] $
          [ grid gridInput ] <>
          ((1 .. floor count) <#> \n ->
            point n { position: rotate (toNumber n * 2.0 * pi / count) `transform` point2 radius 0.0, model2svg })
  , p_ [ input [ type_ InputRange, H.min 0.0, H.max 2.0, step Any, value (show logSpacing)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logSpacing = v })
               ]
       , text " Grid spacing"
       ]
  , p_ [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show logScale)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { logScale = v })
               ]
       , text " Scale"
       ]
  , p_ [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show posX)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posX = v })
               ]
       , text " X"
       ]
  , p_ [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show posY)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { posY = v })
               ]
       , text " Y"
       ]
  , p_ [ input [ type_ InputRange, H.min (-5.0), H.max 5.0, step Any, value (show radius)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { radius = v })
               ]
       , text " Radius"
       ]
  , p_ [ input [ type_ InputRange, H.min 1.0, H.max 100.0, step (Step 1.0), value (show count)
               , onValueInput $ \s -> s # fromString <#> \v -> ChangeState (_ { count = v })
               ]
       , text " Count"
       ]
  ]
  where
    scaling = pow 10.0 logScale

    pos = vec2 posX posY

    size = vec2 777.0 600.0

    gridInput = { gridSpacing: pow 10.0 logSpacing
                , model2svg
                , size
                }

    model2svg = range `containedIn` size

    range = Box { topLeft:     (vec2 (-0.5) (-0.5) - pos) * pure scaling
                , bottomRight: (vec2   0.5    0.5  - pos) * pure scaling
                }

grid :: ∀ m. MonadEffect m => Grid.Input -> H.ComponentHTML Action ChildSlots m
grid input = slot (SProxy :: SProxy "grid") unit Grid.ui input (const Nothing)

point :: ∀ m. MonadEffect m => Int -> Point.Input -> H.ComponentHTML Action ChildSlots m
point id input = slot (SProxy :: SProxy "point") id Point.ui input (const Nothing)

containedIn :: Box Number -> Vec2 Number -> AffineTransform Number
containedIn range size = translate svgCenter * scale scaleMin * translate (-rangeCenter)
  where
    scaleXY = size / boxSize range
    scaleMin = min (_x scaleXY) (_y scaleXY)
    rangeCenter = boxCenter range
    svgCenter = size / pure 2.0
