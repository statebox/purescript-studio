module View.App where

import Prelude hiding (div)

import Data.Array (groupBy, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Either (either, hush)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap, foldr, length, for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.List (List(Nil))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Global (encodeURI)
import Halogen as H
import Halogen.HTML hiding (map, head, i, prop)
import Halogen.HTML.Properties (classes, value, readOnly)
import Halogen.HTML.Events (onValueInput, onClick)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

import Debug.Trace

import Bricks as Bricks
import Bricks
import InferType
import Model

import Output.Haskell (haskellCode)
import Output.JSON (json)

import View.Bricks as Bricks
import View.Term as Term
import View.CopyToClipboard (copyToClipboard)

type State = 
  { input :: Input
  , selectionBox :: Box
  }

_input = prop (SProxy :: SProxy "input")
_pixels = prop (SProxy :: SProxy "pixels")
_context = prop (SProxy :: SProxy "context")
_selectionBox = prop (SProxy :: SProxy "selectionBox")

type Input = 
  { pixels :: String
  , context :: String
  }


data Action
  = UpdatePixels String
  | UpdateContext String
  | BricksMessage Bricks.Output
  | CopyToClipboard String

data Query a
  = DoAction Action a

type ChildSlots = 
  ( bricks :: Bricks.Slot Unit
  , term :: Term.Slot Unit
  )

_bricks = SProxy :: SProxy "bricks"
_term = SProxy :: SProxy "term"


appView :: ∀ o m. MonadEffect m => H.Component HTML Query Input o m
appView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

initialState :: Input -> State
initialState input = { input, selectionBox: { topLeft: 0 /\ 0, bottomRight: 1 /\ 1 } }

render :: ∀ m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render st = div [ classes [ ClassName "app" ] ]
  [ div [ classes [ ClassName "main"] ] 
    [ slot _bricks unit Bricks.bricksView bricksInput (Just <<< BricksMessage)
    , aside []
      [ h2_ [ text "Inferred type" ]
      , div [ classes [ ClassName "fieldset" ]] $
        [ label_ [ text "Whole" ], input [ value termTypeStr, readOnly true ] ]
        <> (selectionType # maybe [] \s -> [ label_ [ text "Selection" ], input [ value s, readOnly true ] ])
      , h2_ [ text "Pixels"]
      , textarea [ value st.input.pixels, onValueInput (Just <<< UpdatePixels) ]
      , h2_ [ text "Context"]
      , textarea [ value st.input.context, onValueInput (Just <<< UpdateContext) ]
      , h2_ [ text "Copy serialized output to clipboard"]
      , div_ $ envE # either (const []) (\env ->
          [ button [ onClick \_ -> Just (CopyToClipboard $ json bricksInput.bricks.term) ]
                   [ text "JSON" ]
          , button [ onClick \_ -> Just (CopyToClipboard $ haskellCode env bricksInput.bricks.term) ]
                   [ text "Haskell" ]
          ])
      ]
    ]
    , h2_ [ text "Term view" ]
    , slot _term unit Term.termView { term: bricksInput.bricks.term, selection: selectionPath } \_ -> Nothing
  ]
  where
    bricksInput :: Bricks.Input
    bricksInput = toBricksInput st.input st.selectionBox

    envE :: String \/ Context String String
    envE = (<>) <$> parseContext st.input.context <*> pure defaultEnv

    termTypeStr = envE # either identity (showInferred <<< inferType bricksInput.bricks.term)
    sub /\ selectionPath = subTerm st.selectionBox bricksInput.bricks.term Nil
    selectionType = hush envE <#> inferType sub <#> showInferred

toBricksInput :: Input -> Box -> Bricks.Input
toBricksInput input selectionBox =
  { bricks, matches, context, selectedBoxes }
  where
    bricks = Bricks.fromPixels (parsePixels input.pixels) (\s -> s == " " || s == "-" || s == "=")

    context = envE # either (const defaultEnv) identity
    envE = (<>) <$> parseContext input.context <*> pure defaultEnv

    typeToMatches (Ty l r) = [Unmatched Valid Input l, Unmatched Valid Output r]
    matches = envE # either (\envError -> Map.empty)
                            (\env -> let inferred = inferType bricks.term env
                                     in fromMatchedVars inferred.bounds (inferred.matches <> typeToMatches inferred.type))

    sub /\ selectionPath = subTerm selectionBox bricks.term Nil
    selectedBoxes = foldMap Set.singleton sub

fromMatchedVars :: ∀ bid. Ord bid => Bounds String bid -> Array (Matches (Var String bid)) -> InputOutput String
fromMatchedVars (Bounds bounds) = foldMap fromMatchedVars' >>> foldr (Map.unionWith (<>)) Map.empty
  where
    fromMatchedVars' :: Matches (Var String bid) -> Array (InputOutput String)
    fromMatchedVars' (Matched ms) = ms 
      # sortBy (\(_ /\ a /\ b) (_ /\ c /\ d) -> comparing varToBox a c <> comparing varToBox b d)
      # groupBy (\(_ /\ a /\ b) (_ /\ c /\ d) -> varToBox a == varToBox c && varToBox b == varToBox d)
      # map toMatch
    fromMatchedVars' (Unmatched val side ms) = ms # groupBy (eq `on` varToBox) # map (toMismatch val side)
    toMatch :: NonEmptyArray (Validity /\ Var String bid /\ Var String bid) -> InputOutput String
    toMatch nonEmpty = Map.fromFoldable 
        [ (lBox /\ Output) /\ leftObjects
        , (rBox /\ Input) /\ rightObjects
        ]
      where
        _ /\ lvar /\ rvar = head nonEmpty
        lBox = varToBox lvar
        rBox = varToBox rvar
        y0 = toNumber $ max (snd lBox.topLeft) (snd rBox.topLeft)
        y1 = toNumber $ min (snd lBox.bottomRight) (snd rBox.bottomRight)
        n = toNumber (length nonEmpty)
        leftObjects /\ rightObjects = nonEmpty # foldMapWithIndex \i (b /\ l /\ r) -> 
          let y = y0 + (y1 - y0) * (0.5 + toNumber i) / n in 
          let ol = getObject l in
          let or = getObject r in
          let validity = if y1 > y0 && (ol == "" || or == "" || ol == or) then b else Invalid in
            [{ y, validity, object: if ol == or then "" else ol }] /\ [{ y, validity, object: or }]
    toMismatch :: Validity -> Side -> NonEmptyArray (Var String bid) -> InputOutput String
    toMismatch validity side nonEmpty = Map.singleton (b /\ side) objects
      where
        b = varToBox (head nonEmpty)
        x = fst $ if side == Input then b.topLeft else b.bottomRight
        y0 = toNumber $ snd b.topLeft
        y1 = toNumber $ snd b.bottomRight
        n = toNumber (length nonEmpty)
        objects = nonEmpty # foldMapWithIndex \i v -> [{ validity, y: y0 + (y1 - y0) * (0.5 + toNumber i) / n, object: getObject v }]
    getObject (BoundVar _ bv) = bv
    getObject (FreeVar fv) = case Map.lookup fv bounds of 
      Just (BoundVar _ bv) -> bv
      _ -> ""
    varToBox :: Var String bid -> Box
    varToBox (BoundVar { box } _) = box
    varToBox (FreeVar (_ /\ { box })) = box

handleAction :: ∀ o m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  UpdatePixels p -> do
    st <- H.modify $ set (_input <<< _pixels) p
    updateWindowLocation st.input
  UpdateContext c -> do
    st <- H.modify $ set (_input <<< _context) c
    updateWindowLocation st.input
  BricksMessage (Bricks.SelectionChanged sel) ->
    H.modify_ $ set _selectionBox sel
  CopyToClipboard s ->
    liftEffect (copyToClipboard s)

handleQuery :: ∀ o m a. MonadEffect m => Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery (DoAction x next) = do
  handleAction x
  pure (Just next)

updateWindowLocation :: ∀ o m. MonadEffect m => Input -> H.HalogenM State Action ChildSlots o m Unit
updateWindowLocation { pixels, context } =
  liftEffect do
    w <- window
    l <- location w
    for_ (encodeURI pixels) \p -> 
      for_ (encodeURI context) \c -> 
        setHash ("pixels=" <> p <> "&context=" <> c) l


parsePixels :: String -> Array (Array String)
parsePixels = map (split (Pattern "")) <<< split (Pattern "\n")
