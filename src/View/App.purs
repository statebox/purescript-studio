module View.App where

import Prelude hiding (div)

import Data.Array (groupBy, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Either (either, hush)
import Data.Foldable (foldMap, foldr, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.Int (toNumber)
import Data.List (List(Nil))
import Data.Map as Map
import Data.Maybe
import Data.Set as Set
import Data.String.Pattern (Pattern(..))
import Data.String.Common (trim, split)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML hiding (map, head, i)
import Halogen.HTML.Properties (classes, value, readOnly)
import Halogen.HTML.Events (onValueInput)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

import Bricks
import InferType
import Model

import View.Bricks as Bricks
import View.Term as Term


type State = 
  { pixels :: String
  , context :: String
  , selectionBox :: Box
  }

type Input = 
  { pixels :: String
  , context :: String
  }


data Action
  = UpdatePixels String
  | UpdateContext String
  | BricksMessage Bricks.Output


type ChildSlots = 
  ( bricks :: Bricks.Slot Unit
  , term :: Term.Slot Unit
  )

_bricks = SProxy :: SProxy "bricks"
_term = SProxy :: SProxy "term"


appView :: ∀ q o m. MonadEffect m => H.Component HTML q Input o m
appView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState { pixels, context } = { pixels, context, selectionBox: { topLeft: 0 /\ 0, bottomRight: 1 /\ 1 } }

render :: ∀ m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render { pixels, context, selectionBox } = div [ classes [ ClassName "app" ] ] 
  [ div [ classes [ ClassName "main"] ] 
    [ slot _bricks unit Bricks.bricksView { 
        bricks, matches, selectedBoxes, 
        context: eEnv # either (\_ -> defaultEnv) identity 
      } (Just <<< BricksMessage)
    , aside []
      [ h2_ [ text "Inferred type" ]
      , div [ classes [ ClassName "fieldset" ]] $
        [ label_ [ text "Whole" ], input [ value result, readOnly true ] ]
        <> (selectionType # maybe [] \s -> [ label_ [ text "Selection" ], input [ value s, readOnly true ] ])
      , h2_ [ text "Pixels"]
      , textarea [ value pixels, onValueInput (Just <<< UpdatePixels) ]
      , h2_ [ text "Context"]
      , textarea [ value context, onValueInput (Just <<< UpdateContext) ]
      ]
    ]
    , h2_ [ text "Term view" ]
    , slot _term unit Term.termView { term: bricks.term, selectionPath } \_ -> Nothing
  ]
  where
    bricks = fromPixels (parsePixels pixels) $ (\s -> s == "0" || s == " " || s == "-" || s == "=")
    eEnv = (<>) <$> parseContext context <*> pure defaultEnv
    typeToMatches (Ty l r) = [Unmatched Valid Input l, Unmatched Valid Output r]
    result /\ matches = eEnv # either (\envError -> envError /\ Map.empty) 
      \env -> let inferred = inferType bricks.term env in 
        showInferred inferred /\ fromMatchedVars inferred.bounds (inferred.matches <> typeToMatches inferred.type)
    sub /\ selectionPath = subTerm selectionBox (bricks.term /\ Nil)
    selectedBoxes = foldMap (\bid -> Set.singleton bid) sub
    selectionType = hush eEnv <#> inferType sub <#> showInferred

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
    st <- H.modify \st -> st { pixels = p }
    updateLocation st
  UpdateContext c -> do
    st <- H.modify \ st -> st { context = c }
    updateLocation st
  BricksMessage (Bricks.SelectionChanged sel) -> H.modify_ \st -> st { selectionBox = sel }

updateLocation :: ∀ o m. MonadEffect m => State -> H.HalogenM State Action ChildSlots o m Unit
updateLocation { pixels, context } =
  liftEffect do
    w <- window
    l <- location w
    setHash ("pixels=" <> pixels <> "&context=" <> context) l


parsePixels :: String -> Array (Array String)
parsePixels = map (split (Pattern "")) <<< split (Pattern "\n") <<< trim
