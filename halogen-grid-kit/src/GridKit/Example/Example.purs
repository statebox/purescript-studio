module GridKit.Example.Example where

import Prelude hiding (div)

import Data.Array ((..))
import Data.Int (toNumber, floor, even)
import Data.Lens (Lens', (+~), (-~), (%~))
import Data.Lens.Record (prop)
import Data.Maybe
import Data.Number (fromString)
import Data.Variant
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
import GridKit.UIComponent
import View.ReactiveInput as ReactiveInput
import View.GridKit.Grid (Grid(..))
import View.GridKit.Point (Point(..))
import View.GridKit.Rect (Rect(..))


type Input = {}

data Action = ChangeState (State -> State)

type State =
  { logSpacing :: Number
  , logScale :: Number
  , posX :: Number
  , posY :: Number
  , radius :: Number
  , count :: Number
  , keyHelpVisible :: Boolean
  }

_logScale :: Lens' State Number
_logScale = prop (SProxy :: SProxy "logScale")

_keyHelpVisible :: Lens' State Boolean
_keyHelpVisible = prop (SProxy :: SProxy "keyHelpVisible")

initialState :: State
initialState =
  { logSpacing: 1.0
  , logScale: 0.0
  , posX: 0.0
  , posY: 0.0
  , radius: 0.5
  , count: 10.0
  , keyHelpVisible: false
  }

type Thing = Variant (point :: Point, rect :: Rect)
_point = SProxy :: SProxy "point"
_rect = SProxy :: SProxy "rect"

type Model =
  { grid   :: Grid
  , things :: Array Thing
  }


ui :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
ui = ReactiveInput.mkComponent
  { initialState
  , render
  , handleAction
  , handleInput: \_ -> pure unit
  }

handleAction :: ∀ m. MonadEffect m => Input -> Action -> H.HalogenM State Action () Void m Unit
handleAction _ (ChangeState f) = H.modify_ f

render :: ∀ m. MonadEffect m => Input -> State -> H.ComponentHTML Action () m
render _ { logSpacing, logScale, posX, posY, radius, count, keyHelpVisible } = div
  [ tabIndex 0, keys.onKeyDown ]
  [ S.svg [ S.width (_x size), S.height (_y size) ] $ toSVG model2svg orderedModel <#> fromPlainHTML
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
  , keys.helpPopup keyHelpVisible
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

    range = Box { topLeft:     pure (-0.5 * scaling) - pos
                , bottomRight: pure ( 0.5 * scaling) - pos
                }

    model :: Model
    model = { grid:   Grid { gridSpacing: pow 10.0 logSpacing, size }
            , things: (1 .. floor count) <#> \n ->
                let center = rotate (toNumber n * 2.0 * pi / count) `transform` point2 radius 0.0 in
                if even n then inj _rect $ Rect $ Box { topLeft: center - vec2 0.1 0.1, bottomRight: center + vec2 0.1 0.1 }
                          else inj _point $ Point center
            }

    orderedModel =
      { _1: model.grid
      , _2: model.things
      }

    zoomInKey = keyHandler
      [ Shortcut metaKey "Equal", Shortcut ctrlKey "Equal"]
      (Just $ text "Zoom in")
      (ChangeState $ _logScale +~ 0.1)
    zoomOutKey = keyHandler
      [ Shortcut metaKey "Minus", Shortcut ctrlKey "Minus"]
      (Just $ text "Zoom out")
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
