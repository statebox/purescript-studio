module View.App where

import Prelude hiding (div)

import Data.Either (either, hush)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap, for_)
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Global (encodeURI)
import Halogen as H
import Halogen.HTML hiding (map, head, i, prop)
import Halogen.HTML.Properties (classes, value, readOnly)
import Halogen.HTML.Events (onValueInput, onClick)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

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
    matches = envE # either (\envError -> [])
                            (\env -> let inferred = inferType bricks.term env
                                     in inferred.matches <> typeToMatches inferred.type)

    sub /\ selectionPath = subTerm selectionBox bricks.term Nil
    selectedBoxes = foldMap Set.singleton sub

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
