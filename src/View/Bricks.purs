module View.Bricks where

import Prelude hiding (div)

import Data.Array ((!!), intercalate, sortWith)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Lens ((+~))
import Data.Lens.Record (prop)
import Data.Map (lookup)
import Data.Maybe
import Data.Set as Set
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, prop, map)
import Halogen.HTML.Properties (classes, tabIndex, ref)
import Halogen.HTML.Events (onKeyDown, onMouseDown, onMouseMove, onMouseUp)
import Halogen.Query.Input (RefLabel(..))
import Svg.Elements as S
import Svg.Attributes hiding (path) as S
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, shiftKey)
import Web.HTML.HTMLElement (focus)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

import Model
import Common (VoidF)


type State = 
  { bricks :: Bricks String
  , matches :: InputOutput String
  , context :: Context String String
  , selectedBoxes :: Set (Brick String)
  , selection :: Box
  , mouseDownFrom :: Maybe Box
  , showWires :: Boolean
  }

_selection = prop (SProxy :: SProxy "selection")
_bottomRight = prop (SProxy :: SProxy "bottomRight")

data Action
  = GetFocus
  | MoveCursorStart (Int /\ Int)
  | MoveCursorEnd (Int /\ Int)
  | Update Input
  | OnKeyDown KeyboardEvent
  | OnMouseDown Box
  | OnMouseMove Box
  | OnMouseUp

type Input = 
  { bricks :: Bricks String
  , matches :: InputOutput String
  , context :: Context String String
  , selectedBoxes :: Set (Brick String)
  }

data Output = SelectionChanged Box

type Slot = H.Slot VoidF Output

bricksView :: ∀ q m. MonadEffect m => H.Component HTML q Input Output m
bricksView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Update, initialize = Just GetFocus }
    }

initialState :: Input -> State
initialState { bricks, matches, context, selectedBoxes } = 
  { bricks
  , matches
  , context
  , selectedBoxes
  , selection: 
    { topLeft: 0 /\ 0
    , bottomRight: 0 /\ 0
    }
  , mouseDownFrom: Nothing
  , showWires: false
  }

render :: ∀ m. State -> H.ComponentHTML Action () m
render { bricks: { width, height, boxes }, selection, matches, context, selectedBoxes, showWires } = div 
  [ ref (RefLabel "bricks")
  , classes [ ClassName "bricks", ClassName $ if showWires then "show-wires" else "show-bricks" ] 
  , tabIndex 0
  , onKeyDown (Just <<< OnKeyDown)
  , onMouseUp (const $ Just $ OnMouseUp)
  ] 
  [ S.svg [ viewBox { topLeft: 0 /\ 0, bottomRight: width /\ height } ] $
    foldMap (\b@{ bid, box } -> let { className, content } = renderBrick matches (lookup bid context) b in [ S.g
      [ svgClasses [ ClassName className, ClassName $ if Set.member b selectedBoxes then "selected" else "" ]
      , onMouseDown (const $ Just $ OnMouseDown box) 
      , onMouseMove (const $ Just $ OnMouseMove box)
      ] 
      ([rect box ""] <> content)]) boxes
    <> [rect (selectionBox selection) "selection"]
  ]

renderBrick :: ∀ m. InputOutput String -> Maybe (TypeDecl String) -> Brick String 
  -> { className :: String, content :: Array (H.ComponentHTML Action () m) }
renderBrick matches (Just (Gen _)) b@{ box } = 
  { className: "box"
  , content: 
      renderBox b
      <> maybe [] (foldMap (renderLines true Input b)) (lookup (box /\ Input) matches)
      <> maybe [] (foldMap (renderLines true Output b)) (lookup (box /\ Output) matches)
  }
renderBrick matches (Just (Perm perm)) b = { className: "wires", content: renderPerm matches b perm }
renderBrick matches (Just (Spider c _ _)) b@{ box } = 
  { className: "wires"
  , content: 
      maybe [] (foldMap (renderLines false Input b)) (lookup (box /\ Input) matches) <>
      maybe [] (foldMap (renderLines false Output b)) (lookup (box /\ Output) matches) <>
      renderNode b c
  }
renderBrick _ Nothing _ = { className: "box", content: [] }

renderBox :: ∀ m. Brick String -> Array (H.ComponentHTML Action () m)
renderBox { bid, box: { topLeft: xl /\ yt, bottomRight: xr /\ yb }} = 
  [ S.rect [ S.x (mx - 0.18), S.y (my - 0.25), S.width 0.36, S.height 0.5, svgClasses [ ClassName "inner-box" ] ]
  , S.text 
    [ S.x mx, S.y (my + 0.12)
    , S.attr (AttrName "text-anchor") "middle"
    , svgClasses [ ClassName "inner-box-text" ] 
    ] [ text bid ]
  ]
  where
    mx = (toNumber xl + toNumber xr) / 2.0
    my = (toNumber yt + toNumber yb) / 2.0

renderNode :: ∀ m. Brick String -> Color -> Array (H.ComponentHTML Action () m)
renderNode { bid, box: { topLeft: xl /\ yt, bottomRight: xr /\ yb }} color = 
  [ S.circle [ S.cx mx, S.cy my, S.r 0.05, svgClasses [ ClassName "node", ClassName (show color) ] ]
  ]
  where
    mx = (toNumber xl + toNumber xr) / 2.0
    my = (toNumber yt + toNumber yb) / 2.0

renderLines :: ∀ m. Boolean -> Side -> Brick String -> Match String -> Array (H.ComponentHTML Action () m)
renderLines toBox side { box: { topLeft: xl /\ yt, bottomRight: xr /\ yb }} m@{ y, validity } = 
  [ S.g [ svgClasses [ ClassName "object", validityClassName validity ] ] $
    [ S.path [ svgClasses [ ClassName "line" ], S.d [ S.Abs (S.M x y), S.Abs (S.C cpx y cpx cpy mx cpy) ] ]
    ] <> renderObject side x m
  ]
  where 
    x = toNumber $ if side == Input then xl else xr
    mx = (toNumber xl + toNumber xr) / 2.0 + if toBox then if side == Input then -0.18 else 0.18 else 0.0
    my = (toNumber yt + toNumber yb) / 2.0
    height = toNumber yt - toNumber yb
    cpx = (mx + x) / 2.0
    cpy = (my + (my - y) * (if toBox then 0.3 else 0.05) / height)

renderObject :: ∀ m. Side -> Number -> Match String -> Array (H.ComponentHTML Action () m)
renderObject Input x { y, validity, object } = 
  [ S.text
    [ S.x (x + 0.05), S.y (y - 0.05)
    , S.attr (AttrName "text-anchor") "start"
    , svgClasses [ ClassName "object", validityClassName validity ]
    ] [ text object ]
  , S.path [ S.d [ S.Abs (S.M (x - 0.001) (y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false false 0.0 0.08) ] ]
  ]
renderObject Output x { y, validity, object } = 
  [ S.text
    [ S.x (x - 0.05), S.y (y - 0.05)
    , S.attr (AttrName "text-anchor") "end"
    , svgClasses [ ClassName "object", validityClassName validity ]
    ] [ text object ]
  , S.path [ S.d [ S.Abs (S.M (x + 0.001) (y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false true 0.0 0.08) ] ]
  ]

renderPerm :: ∀ m. InputOutput String -> Brick String -> Array Int -> Array (H.ComponentHTML Action () m)
renderPerm matches { box: b@{ topLeft: xl /\ yt, bottomRight: xr /\ yb } } perm = 
  case lookup (b /\ Input) matches <#> sortWith (_.y), lookup (b /\ Output) matches <#> sortWith (_.y) of
    Just yls, Just yrs ->
      perm # foldMapWithIndex \r l -> fromMaybe [S.path []] $ do
        ml@{ y: yl } <- yls !! (l - 1)
        mr@{ y: yr } <- yrs !! r
        pure $
          [ S.path 
              [ S.d [ S.Abs (S.M xln yl), S.Abs (S.C cpx yl cpx yr xrn yr) ] 
              , svgClasses [ ClassName "line" ]
              ]
          ] <> renderObject Input xln ml <> renderObject Output xrn mr
    _, _ -> []
  where 
    xln = toNumber xl
    xrn = toNumber xr
    cpx = (xln + xrn) / 2.0
  
sideClassName :: Side -> ClassName
sideClassName side = ClassName $ if side == Input then "input" else "output"

validityClassName :: Validity -> ClassName
validityClassName validity = ClassName $ if validity == Valid then "valid" else "invalid"

selectionBox :: Box -> Box
selectionBox selection = { topLeft, bottomRight }
  where
    { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } = selection
    topLeft = min x0 x1 /\ min y0 y1
    bottomRight = (max x0 x1 + 1) /\ (max y0 y1 + 1)


handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Update { bricks, matches, context, selectedBoxes } -> H.modify_ \st -> 
    st { bricks = bricks, matches = matches, context = context, selectedBoxes = selectedBoxes }
  GetFocus -> do
    mb <- H.getRef (RefLabel "bricks")
    maybe (pure unit) (liftEffect <<< focus <<< unsafeCoerce) mb
  MoveCursorStart d -> updateSelection \sel ->
    { topLeft: moveCursor d sel.topLeft sel.bottomRight
    , bottomRight: moveCursor d sel.bottomRight sel.topLeft 
    }
  MoveCursorEnd d -> updateSelection (_bottomRight +~ d)
  OnKeyDown k -> let act dx dy = handleAction $ (if shiftKey k then MoveCursorEnd else MoveCursorStart) (dx /\ dy) in
    case code k of
      "ArrowLeft" -> act (-1) 0
      "ArrowUp" -> act 0 (-1)
      "ArrowRight" -> act 1 0
      "ArrowDown" -> act 0 1
      "AltLeft" -> H.modify_ \st -> st { showWires = not st.showWires }
      "AltRight" -> H.modify_ \st -> st { showWires = not st.showWires }
      x -> trace x pure
  OnMouseDown b@{ topLeft, bottomRight } -> do
    H.modify_ \st -> st { mouseDownFrom = Just b }
    updateSelection \_ -> { topLeft, bottomRight: bottomRight - (1 /\ 1) }
  OnMouseMove b1 -> do
    mb0 <- H.gets _.mouseDownFrom
    case mb0 of
      Nothing -> pure unit
      Just b0 -> do
        updateSelection \_ -> {
          topLeft: min (fst b0.topLeft) (fst b1.topLeft) /\ min (snd b0.topLeft) (snd b1.topLeft),
          bottomRight: (max (fst b0.bottomRight) (fst b1.bottomRight) - 1) /\ (max (snd b0.bottomRight) (snd b1.bottomRight) - 1)
        }
  OnMouseUp -> H.modify_ \st -> st { mouseDownFrom = Nothing }

updateSelection :: ∀ m. (Box -> Box) -> H.HalogenM State Action () Output m Unit
updateSelection f = do
  { selection, bricks: { width, height }} <- H.get
  let { topLeft, bottomRight } = f selection
  let selection' = { topLeft: clamp2d width height topLeft, bottomRight: clamp2d width height bottomRight }
  H.modify_ \st -> st { selection = selection' } 
  H.raise (SelectionChanged $ selectionBox selection')

clamp2d :: Int -> Int -> Int /\ Int -> Int /\ Int
clamp2d width height (x /\ y) = clamp 0 (width - 1) x /\ clamp 0 (height - 1) y

moveCursor :: Int /\ Int -> Int /\ Int -> Int /\ Int -> Int /\ Int
moveCursor (dx /\ dy) (x0 /\ y0) (x1 /\ y1) = move dx x0 x1 /\ move dy y0 y1
  where
    move d a b | a == b = a + d
    move -1 a b = min a b
    move 1 a b = max a b
    move _ a _ = a

rect :: ∀ m. Box -> String -> H.ComponentHTML Action () m
rect { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } cls = S.rect $
  [ S.x (toNumber x0 + 0.005)
  , S.y (toNumber y0 + 0.005)
  , S.width (toNumber (x1 - x0) - 0.01)
  , S.height (toNumber (y1 - y0) - 0.01)
  , S.rx 0.07
  , svgClasses [ ClassName cls ]
  ]

gridPosition :: ∀ r i. Box -> IProp r i
gridPosition { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } =
  attr (AttrName "style") $ 
    "grid-column-start: " <> show (x0 + 1) <>
    "; grid-row-start: " <> show (y0 + 1) <> 
    "; grid-column-end: " <> show (x1 + 1) <>
    "; grid-row-end: " <> show (y1 + 1)

viewBox :: ∀ r i. Box -> IProp (viewBox :: String | r) i
viewBox { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } =
  S.viewBox (toNumber x0 - 0.01) (toNumber y0 - 0.01) (toNumber (x1 - x0) + 0.02) (toNumber (y1 - y0) + 0.02)

svgClasses :: ∀ r i. Array (ClassName) -> IProp r i
svgClasses arr = S.attr (AttrName "class") $ intercalate " " $ map (\(ClassName s) -> s) arr
