module View.Bricks where

import Prelude hiding (div)

import Data.Array ((!!), intercalate, sortWith, groupBy, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Foldable (foldMap, foldr, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Lens (Lens, (+~))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Map (Map, lookup)
import Data.Maybe
import Data.Set as Set
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map)
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

type Match bv = { y :: Number, validity :: Validity, center :: Boolean, object :: bv }
type InputOutput bv = Map (Box /\ Side) (Array (Match bv))

type State =
  { input :: Input
  , selection :: Box
  , mouseDownFrom :: Maybe Box
  , showWires :: Boolean
  }

_selection :: ∀ a b r. Lens { selection :: a | r } { selection :: b | r } a b
_selection = prop (SProxy :: SProxy "selection")

_bottomRight :: ∀ a b r. Lens { bottomRight :: a | r } { bottomRight :: b | r } a b
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
  , matches :: Array (Matches (VarWithBox String))
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
initialState input =
  { input
  , selection:
    { topLeft: 0 /\ 0
    , bottomRight: 0 /\ 0
    }
  , mouseDownFrom: Nothing
  , showWires: false
  }

render :: ∀ m. State -> H.ComponentHTML Action () m
render { input: { bricks: { width, height, boxes }, matches, context, selectedBoxes }, selection, showWires } = div
  [ ref (RefLabel "bricks")
  , classes [ ClassName "bricks", ClassName $ if showWires then "show-wires" else "show-bricks" ]
  , tabIndex 0
  , onKeyDown (Just <<< OnKeyDown)
  , onMouseUp (const $ Just $ OnMouseUp)
  ]
  [ S.svg [ viewBox { topLeft: 0 /\ 0, bottomRight: width /\ height } ] $
    foldMap (\b@{ bid, box } -> let { className, content } = renderBrick (matchesToIO matches) (lookup bid context) b in [ S.g
      [ svgClasses [ ClassName className, ClassName $ if Set.member b selectedBoxes then "selected" else "" ]
      , onMouseDown (const $ Just $ OnMouseDown box)
      , onMouseMove (const $ Just $ OnMouseMove box)
      ]
      ([rect box ""] <> content)]) boxes
    <> [rect (selectionBox selection) "selection"]
    <> [ S.defs [] [
      S.marker [ S.id "arrowhead", S.refX 3.0, S.refY 4.0, S.markerUnits S.StrokeWidth, S.markerWidth 6.0, S.markerHeight 8.0, S.orient S.AutoOrient ] [
        S.path [ svgClasses [ ClassName "arrowhead" ], S.d [ S.Abs (S.M 0.0 0.0), S.Abs (S.L 6.0 4.0), S.Abs (S.L 0.0 8.0), S.Abs (S.L 2.0 4.0), S.Abs S.Z ] ]
      ],
      S.marker [ S.id "arrowheadrev", S.refX 3.0, S.refY 4.0, S.markerUnits S.StrokeWidth, S.markerWidth 6.0, S.markerHeight 8.0, S.orient S.AutoOrient ] [
        S.path [ svgClasses [ ClassName "arrowhead" ], S.d [ S.Abs (S.M 6.0 0.0), S.Abs (S.L 0.0 4.0), S.Abs (S.L 6.0 8.0), S.Abs (S.L 4.0 4.0), S.Abs S.Z ] ]
      ]
    ] ]
  ]

renderBrick :: ∀ m. InputOutput String -> Maybe (TypeDecl String) -> Brick String
  -> { className :: String, content :: Array (H.ComponentHTML Action () m) }
renderBrick io (Just (Gen _)) b@{ box } =
  { className: "box"
  , content:
      renderBox b
      <> maybe [] (foldMap (renderLines true Input b)) (lookup (box /\ Input) io)
      <> maybe [] (foldMap (renderLines true Output b)) (lookup (box /\ Output) io)
  }
renderBrick io (Just (Perm perm)) b = { className: "wires", content: renderPerm io b perm }
renderBrick io (Just (Spider c _ _)) b@{ box } =
  { className: "wires"
  , content:
      maybe [] (foldMap (renderLines false Input b)) (lookup (box /\ Input) io) <>
      maybe [] (foldMap (renderLines false Output b)) (lookup (box /\ Output) io) <>
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
renderLines toBox side { box: { topLeft: xl /\ yt, bottomRight: xr /\ yb }} m@{ y } =
  [ S.g [ svgClasses (objectClassNames m) ] $
    (if not toBox && m.center then [] else renderObject side x m) <>
    [ S.path
      [ svgClasses [ ClassName "line" ]
      , S.d [ S.Abs (S.M mx cpy), S.Abs (S.C cpx cpy cpx y x y) ]
      , S.markerEnd (if m.center && m.validity == Valid then
          if isBackwards m.object == (side == Input) then "url(#arrowhead)" else "url(#arrowheadrev"
        else "")
      ]
    ]
  ]
  where
    x = toNumber $ if side == Input then xl else xr
    mx = (toNumber xl + toNumber xr) / 2.0 + if toBox then if side == Input then -0.18 else 0.18 else 0.0
    my = (toNumber yt + toNumber yb) / 2.0
    height = toNumber yt - toNumber yb
    cpx = (mx + x) / 2.0
    cpy = (my + (my - y) * (if toBox then 0.3 else 0.05) / height)

renderObject :: ∀ m. Side -> Number -> Match String -> Array (H.ComponentHTML Action () m)
renderObject Input x m =
  [ S.text
    [ S.x x, S.y m.y
    , S.attr (AttrName "text-anchor") "start"
    , svgClasses (objectClassNames m)
    ] [ text m.object ]
  ] <> if m.center then []
  else [ S.path [ S.d [ S.Abs (S.M (x - 0.001) (m.y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false false 0.0 0.08) ] ] ]
renderObject Output x m =
  [ S.text
    [ S.x x, S.y m.y
    , S.attr (AttrName "text-anchor") "end"
    , svgClasses (objectClassNames m)
    ] [ text m.object ]
  ] <> if m.center then []
  else [ S.path [ S.d [ S.Abs (S.M (x + 0.001) (m.y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false true 0.0 0.08) ] ] ]

renderPerm :: ∀ m. InputOutput String -> Brick String -> Array Int -> Array (H.ComponentHTML Action () m)
renderPerm io { box: b@{ topLeft: xl /\ yt, bottomRight: xr /\ yb } } perm =
  case lookup (b /\ Input) io <#> sortWith _.y, lookup (b /\ Output) io <#> sortWith _.y of
    Just yls, Just yrs ->
      perm # foldMapWithIndex \r l -> fromMaybe [S.path []] $ do
        ml <- yls !! (l - 1)
        mr <- yrs !! r
        pure $
          [ S.path
              [ S.d [ S.Abs (S.M xln ml.y), S.Abs (S.C cpx ml.y cpx mr.y xrn mr.y) ]
              , svgClasses [ ClassName "line" ]
              ]
          ] <> (if ml.center then [] else renderObject Input xln ml) <> (if mr.center then [] else renderObject Output xrn mr)
    _, _ -> []
  where
    xln = toNumber xl
    xrn = toNumber xr
    cpx = (xln + xrn) / 2.0

sideClassName :: Side -> ClassName
sideClassName side = ClassName $ if side == Input then "input" else "output"

objectClassNames :: Match String -> Array ClassName
objectClassNames { validity, center } =
  [ ClassName "object"
  , ClassName $ if validity == Valid then "valid" else "invalid"
  ] <> if center then [ClassName "centered"] else []

selectionBox :: Box -> Box
selectionBox selection = { topLeft, bottomRight }
  where
    { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } = selection
    topLeft = min x0 x1 /\ min y0 y1
    bottomRight = (max x0 x1 + 1) /\ (max y0 y1 + 1)


handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Update input ->
    H.modify_ $ \st -> st { input = input }
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
  OnMouseUp ->
    H.modify_ $ \st -> st { mouseDownFrom = Nothing }

updateSelection :: ∀ m. (Box -> Box) -> H.HalogenM State Action () Output m Unit
updateSelection f = do
  { selection, input: { bricks: { width, height } } } <- H.get
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


matchesToIO :: Array (Matches (VarWithBox String)) -> InputOutput String
matchesToIO = foldMap matchesToIO' >>> foldr (Map.unionWith (<>)) Map.empty
  where
    matchesToIO' :: Matches (VarWithBox String) -> Array (InputOutput String)
    matchesToIO' (Matched ms) = ms
      # sortBy (\(_ /\ a /\ b) (_ /\ c /\ d) -> comparing _.box a c <> comparing _.box b d)
      # groupBy (\(_ /\ a /\ b) (_ /\ c /\ d) -> a.box == c.box && b.box == d.box)
      # map toMatch
    matchesToIO' (Unmatched val side ms) = ms # groupBy (eq `on` _.box) # map (toMismatch val side)
    toMatch :: NonEmptyArray (Validity /\ VarWithBox String /\ VarWithBox String) -> InputOutput String
    toMatch nonEmpty = Map.fromFoldable
        [ (lBox /\ Output) /\ leftObjects
        , (rBox /\ Input) /\ rightObjects
        ]
      where
        _ /\ lvar /\ rvar = head nonEmpty
        lBox = lvar.box
        rBox = rvar.box
        y0 = toNumber $ max (snd lBox.topLeft) (snd rBox.topLeft)
        y1 = toNumber $ min (snd lBox.bottomRight) (snd rBox.bottomRight)
        n = toNumber (length nonEmpty)
        leftObjects /\ rightObjects = nonEmpty # foldMapWithIndex \i (b /\ l /\ r) ->
          let y = y0 + (y1 - y0) * (0.5 + toNumber i) / n in
          let ol = getObject l in
          let or = getObject r in
          let center = ol == or in
          let validity = if y1 > y0 && (ol == "" || or == "" || ol == or) then b else Invalid in
            [{ y, validity, center, object: ol }] /\ [{ y, validity, center, object: or }]
    toMismatch :: Validity -> Side -> NonEmptyArray (VarWithBox String) -> InputOutput String
    toMismatch validity side nonEmpty = Map.singleton (b /\ side) objects
      where
        b = (head nonEmpty).box
        x = fst $ if side == Input then b.topLeft else b.bottomRight
        y0 = toNumber $ snd b.topLeft
        y1 = toNumber $ snd b.bottomRight
        n = toNumber (length nonEmpty)
        objects = nonEmpty # foldMapWithIndex \i v -> [{ validity, y: y0 + (y1 - y0) * (0.5 + toNumber i) / n, object: getObject v, center: false }]
    getObject { var: BoundVar bv } = bv
    getObject _ = ""
