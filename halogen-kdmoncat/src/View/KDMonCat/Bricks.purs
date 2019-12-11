module View.KDMonCat.Bricks where

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
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (vec2, _x, _y, binOp)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (code, head, prop, map)
import Halogen.HTML.Properties (classes, tabIndex, ref)
import Halogen.HTML.Events (onKeyDown, onMouseDown, onMouseMove, onMouseUp)
import Halogen.Query.Input (RefLabel(..))
import Svg.Elements as S
import Svg.Attributes hiding (path) as S
import Web.DOM (Element)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, shiftKey, toEvent)
import Web.HTML.HTMLElement (HTMLElement, focus)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

import KDMonCat.Model
import KDMonCat.Common (VoidF, Disc2)

import View.KDMonCat.Box as Box


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
  | MoveCursorStart Disc2
  | MoveCursorEnd Disc2
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

type ChildSlots =
  ( box :: Box.Slot Action Disc2
  )

_box = SProxy :: SProxy "box"

type Slot = H.Slot VoidF Output

bricksView :: ∀ q m. MonadEffect m => H.Component HTML q Input Output m
bricksView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Update }
    }

initialState :: Input -> State
initialState input =
  { input
  , selection:
    { topLeft: zero
    , bottomRight: zero
    }
  , mouseDownFrom: Nothing
  , showWires: true
  }

render :: ∀ m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render { input: { bricks: { width, height, boxes }, matches, context, selectedBoxes }, selection, showWires } = div
  [ ref (RefLabel "bricks")
  , classes [ ClassName "bricks", ClassName $ if showWires then "show-wires" else "show-bricks" ]
  , tabIndex 0
  , onKeyDown (Just <<< OnKeyDown)
  , onMouseUp (const $ Just $ OnMouseUp)
  ]
  [ S.svg [ viewBox { topLeft: vec2 0 0, bottomRight: vec2 width height } ] $
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

renderBrick :: ∀ m. MonadEffect m => InputOutput String -> Maybe { name :: String, type :: TypeDecl String } -> Brick String
  -> { className :: String, content :: Array (H.ComponentHTML Action ChildSlots m) }
renderBrick io (Just { name, type: Gen _ }) b@{ box } =
  { className: "box"
  , content:
      maybe [] (foldMap (renderLines genLineSettings Input b)) (lookup (box /\ Input) io) <>
      maybe [] (foldMap (renderLines genLineSettings Output b)) (lookup (box /\ Output) io) <>
      renderBox name box
  }
renderBrick io (Just { type: Perm perm }) b = { className: "wires", content: renderPerm io b perm }
renderBrick io (Just { type: Spider c _ _ }) b@{ box } =
  { className: "wires"
  , content:
      maybe [] (foldMap (renderLines spiderLineSettings Input b)) (lookup (box /\ Input) io) <>
      maybe [] (foldMap (renderLines spiderLineSettings Output b)) (lookup (box /\ Output) io) <>
      renderNode b c
  }
renderBrick io (Just { type: Cup }) b@{ box } =
  { className: "wires"
  , content:
      maybe [] (foldMap (renderLines cupcapLineSettings Input b)) (lookup (box /\ Input) io) <>
      maybe [] (foldMap (renderLines cupcapLineSettings Output b)) (lookup (box /\ Output) io)
  }
renderBrick io (Just { type: Cap }) b@{ box } =
  { className: "wires"
  , content:
      maybe [] (foldMap (renderLines cupcapLineSettings Input b)) (lookup (box /\ Input) io) <>
      maybe [] (foldMap (renderLines cupcapLineSettings Output b)) (lookup (box /\ Output) io)
  }
renderBrick _ Nothing _ = { className: "box", content: [] }

renderBox :: ∀ m. MonadEffect m => String -> Box -> Array (H.ComponentHTML Action ChildSlots m)
renderBox name { topLeft, bottomRight } =
  [ slot _box topLeft Box.boxView
    { content: S.text
      [ S.x mx, S.y (my + 0.14)
      , S.attr (AttrName "text-anchor") "middle"
      , svgClasses [ ClassName "inner-box-text" ]
      ] [ text name ]
    , minWidth: 0.33
    , maxWidth: 0.5
    , minHeight: 0.5
    , maxHeight: 0.5
    , padding: 0.015
    , className: "inner-box"
    }
    Just
  ]
  where
    center = map toNumber (topLeft + bottomRight) / pure 2.0
    mx = _x center
    my = _y center

renderNode :: ∀ m. Brick String -> Color -> Array (H.ComponentHTML Action ChildSlots m)
renderNode { bid, box: { topLeft, bottomRight } } color =
  [ S.circle [ S.cx mx, S.cy my, S.r 0.05, svgClasses [ ClassName "node", ClassName (show color) ] ]
  ]
  where
    center = map toNumber (topLeft + bottomRight) / pure 2.0
    mx = _x center
    my = _y center

type LineSettings =
  { toBox :: Boolean
  , cpxf :: Number -> Number
  , cpyf :: Number -> Number
  }

genLineSettings :: LineSettings
genLineSettings = { toBox: true, cpxf: \dx -> dx / 2.0, cpyf: \dy -> dy * 0.3 }
spiderLineSettings :: LineSettings
spiderLineSettings = { toBox: false, cpxf: \dx -> dx / 2.0, cpyf: \dy -> dy * 0.05 }
cupcapLineSettings :: LineSettings
cupcapLineSettings = { toBox: false, cpxf: \dx -> 0.0, cpyf: \dy -> 0.0 }

renderLines :: ∀ m. LineSettings -> Side -> Brick String -> Match String -> Array (H.ComponentHTML Action ChildSlots m)
renderLines { toBox, cpxf, cpyf } side { box: { topLeft, bottomRight } } m@{ y } =
  [ S.g [ svgClasses (objectClassNames m) ] $
    (if not toBox && m.center then [] else renderObject side x m) <>
    [ S.path
      [ svgClasses [ ClassName "line" ]
      , S.d [ S.Abs (S.M mx cpy), S.Abs (S.C cpx cpy cpx y x y) ]
      , S.markerEnd (if m.center && m.validity == Valid then
          if isBackwards m.object == (side == Input) then "url(#arrowhead)" else "url(#arrowheadrev)"
        else "")
      ]
    ]
  ]
  where
    xl = toNumber (_x topLeft)
    yt = toNumber (_y topLeft)
    xr = toNumber (_x bottomRight)
    yb = toNumber (_y bottomRight)
    x = if side == Input then xl else xr
    mx = (xl + xr) / 2.0 + if toBox then if side == Input then -0.18 else 0.18 else 0.0
    my = (yt + yb) / 2.0
    height = yb - yt
    cpx = mx + cpxf (x - mx)
    cpy = my + cpyf ((y - my) / height)

renderObject :: ∀ m. Side -> Number -> Match String -> Array (H.ComponentHTML Action ChildSlots m)
renderObject Input x m =
  [ S.text
    [ S.x x, S.y m.y
    , S.attr (AttrName "text-anchor") "start"
    , svgClasses (objectClassNames m)
    ] [ text (typeName m) ]
  ] <> if m.center then []
  else [ S.path [ S.d [ S.Abs (S.M (x - 0.001) (m.y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false false 0.0 0.08) ] ] ]
renderObject Output x m =
  [ S.text
    [ S.x x, S.y m.y
    , S.attr (AttrName "text-anchor") "end"
    , svgClasses (objectClassNames m)
    ] [ text (typeName m) ]
  ] <> if m.center then []
  else [ S.path [ S.d [ S.Abs (S.M (x + 0.001) (m.y - 0.04)), S.Rel (S.A 0.04 0.04 180.0 false true 0.0 0.08) ] ] ]

typeName :: Match String -> String
typeName { object, validity: Invalid } = object
typeName { object, validity: Valid } = makeForwards object

renderPerm :: ∀ m. InputOutput String -> Brick String -> Array Int -> Array (H.ComponentHTML Action ChildSlots m)
renderPerm io { box: b } perm =
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
    xln = toNumber (_x b.topLeft)
    xrn = toNumber (_x b.bottomRight)
    cpx = (xln + xrn) / 2.0

sideClassName :: Side -> ClassName
sideClassName side = ClassName $ if side == Input then "input" else "output"

objectClassNames :: Match String -> Array ClassName
objectClassNames { validity, center } =
  [ ClassName "object"
  , ClassName $ if validity == Valid then "valid" else "invalid"
  ] <> if center then [ClassName "centered"] else []

selectionBox :: Box -> Box
selectionBox { topLeft, bottomRight } =
  { topLeft:     binOp min topLeft bottomRight
  , bottomRight: binOp max topLeft bottomRight + vec2 1 1
  }


handleAction :: ∀ m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Update input ->
    H.modify_ $ \st -> st { input = input }
  GetFocus -> do
    mb <- H.getRef (RefLabel "bricks")
    mb # maybe (pure unit) (toHTMLElement >>> focus >>> liftEffect)
  MoveCursorStart d -> updateSelection \sel ->
    { topLeft: moveCursor d sel.topLeft sel.bottomRight
    , bottomRight: moveCursor d sel.bottomRight sel.topLeft
    }
  MoveCursorEnd d -> updateSelection (_bottomRight +~ d)
  OnKeyDown e -> let act dx dy = handleAction $ (if shiftKey e then MoveCursorEnd else MoveCursorStart) (vec2 dx dy) in do
    case code e of
      "ArrowLeft" -> act (-1) 0
      "ArrowUp" -> act 0 (-1)
      "ArrowRight" -> act 1 0
      "ArrowDown" -> act 0 1
      "AltLeft" -> H.modify_ \st -> st { showWires = not st.showWires }
      "AltRight" -> H.modify_ \st -> st { showWires = not st.showWires }
      x -> trace x pure
    preventDefault (toEvent e) # liftEffect
  OnMouseDown b@{ topLeft, bottomRight } -> do
    H.modify_ \st -> st { mouseDownFrom = Just b }
    updateSelection \_ -> { topLeft, bottomRight: bottomRight - vec2 1 1 }
  OnMouseMove b1 -> do
    mb0 <- H.gets _.mouseDownFrom
    case mb0 of
      Nothing -> pure unit
      Just b0 -> do
        updateSelection \_ ->
          { topLeft: binOp min b0.topLeft b1.topLeft
          , bottomRight: binOp max b0.bottomRight b1.bottomRight - vec2 1 1
          }
  OnMouseUp ->
    H.modify_ $ \st -> st { mouseDownFrom = Nothing }

updateSelection :: ∀ m. (Box -> Box) -> H.HalogenM State Action ChildSlots Output m Unit
updateSelection f = do
  { selection, input: { bricks: { width, height } } } <- H.get
  let { topLeft, bottomRight } = f selection
  let selection' = { topLeft: clamp2d width height topLeft, bottomRight: clamp2d width height bottomRight }
  H.modify_ \st -> st { selection = selection' }
  H.raise (SelectionChanged $ selectionBox selection')

clamp2d :: Int -> Int -> Disc2 -> Disc2
clamp2d width height p = clamp <$> pure 0 <*> vec2 (width - 1) (height - 1) <*> p

moveCursor :: Disc2 -> Disc2 -> Disc2 -> Disc2
moveCursor d2 p0 p1 = move <$> d2 <*> p0 <*> p1
  where
    move d a b | a == b = a + d
    move -1 a b = min a b
    move 1 a b = max a b
    move _ a _ = a

rect :: ∀ m. Box -> String -> H.ComponentHTML Action ChildSlots m
rect { topLeft: p0, bottomRight: p1 } cls = let dp = p1 - p0 in S.rect $
  [ S.x (toNumber (_x p0) + 0.005)
  , S.y (toNumber (_y p0) + 0.005)
  , S.width (toNumber (_x dp) - 0.01)
  , S.height (toNumber (_y dp) - 0.01)
  , S.rx 0.07
  , svgClasses [ ClassName cls ]
  ]

viewBox :: ∀ r i. Box -> IProp (viewBox :: String | r) i
viewBox { topLeft: p0, bottomRight: p1 } = let dp = p1 - p0 in
  S.viewBox (toNumber (_x p0) - 0.01) (toNumber (_y p0) - 0.01) (toNumber (_x dp) + 0.02) (toNumber (_y dp) + 0.02)

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
        y0 = toNumber $ max (_y lBox.topLeft) (_y rBox.topLeft)
        y1 = toNumber $ min (_y lBox.bottomRight) (_y rBox.bottomRight)
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
        x = _x (if side == Input then b.topLeft else b.bottomRight)
        y0 = toNumber $ _y b.topLeft
        y1 = toNumber $ _y b.bottomRight
        n = toNumber (length nonEmpty)
        objects = nonEmpty # foldMapWithIndex \i v -> [{ validity, y: y0 + (y1 - y0) * (0.5 + toNumber i) / n, object: getObject v, center: false }]
    getObject { var: BoundVar bv } = bv
    getObject _ = ""

--------------------------------------------------------------------------------

toHTMLElement :: Element -> HTMLElement
toHTMLElement = unsafeCoerce