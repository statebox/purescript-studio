module View.KDMonCat.App where

import Prelude hiding (div)

import Data.Either (either, hush)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap, for_)
import Data.Lens (Lens, set)
import Data.Lens.Record (prop)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect, liftEffect)
import Global (encodeURIComponent)
import Halogen as H
import Halogen.HTML hiding (map, head, i, prop)
import Halogen.HTML.Properties (classes, value, readOnly, href, type_, InputType(InputText))
import Halogen.HTML.Events (onValueInput, onClick)
import View.ReactiveInput as ReactiveInput
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

import KDMonCat.Bricks as Bricks
import KDMonCat.InferType
import KDMonCat.Model

import KDMoncat.Input.String as String
import KDMonCat.Output.Haskell (haskellCode)
import KDMonCat.Output.JSON (json)

import View.KDMonCat.Bricks as Bricks
import View.KDMonCat.Term as Term
import View.KDMonCat.CopyToClipboard (copyToClipboard)

type Input = String.Input

type State =
  { input :: Input
  , selectionBox :: Box
  }

_input :: ∀ a b r. Lens { input :: a | r } { input :: b | r } a b
_input = prop (SProxy :: SProxy "input")

_pixels :: ∀ a b r. Lens { pixels :: a | r } { pixels :: b | r } a b
_pixels = prop (SProxy :: SProxy "pixels")

_context :: ∀ a b r. Lens { context :: a | r } { context :: b | r } a b
_context = prop (SProxy :: SProxy "context")

_selectionBox :: ∀ a b r. Lens { selectionBox :: a | r } { selectionBox :: b | r } a b
_selectionBox = prop (SProxy :: SProxy "selectionBox")


type Slot = H.Slot Query Void

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
  ReactiveInput.mkComponentWithQuery
    { initialState
    , render
    , handleAction: \_ -> handleAction
    , handleQuery
    , handleInput
    }

initialState :: State
initialState = { input: mempty, selectionBox: { topLeft: zero, bottomRight: zero } }

render :: ∀ m. MonadEffect m => Input -> State -> H.ComponentHTML Action ChildSlots m
render _ st = div [ classes [ ClassName "kdmoncat-app" ] ]
  [ div [ classes [ ClassName "main"] ]
    [ slot _bricks unit Bricks.bricksView bricksInput (Just <<< BricksMessage)
    , aside []
      [ h2_ [ text "Inferred type" ]
      , div [ classes [ ClassName "fieldset" ]] $
        [ label_ [ text "Whole" ], input [ type_ InputText, value termTypeStr, readOnly true ] ]
        <> (selectionType # maybe [] \s -> [ label_ [ text "Selection" ], input [ type_ InputText, value s, readOnly true ] ])
      , h2_ [ text "Pixels"]
      , textarea [ value st.input.pixels, onValueInput (Just <<< UpdatePixels) ]
      , h2_ [ text "Context"]
      , textarea [ value st.input.context, onValueInput (Just <<< UpdateContext) ]
      , h2_ [ text "Copy serialized output to clipboard"]
      , div_ $ inferredType # either (const []) (\{ term } ->
          [ button [ onClick \_ -> Just (CopyToClipboard $ json term) ]
                    [ text "JSON" ]
          , button [ onClick \_ -> Just (CopyToClipboard $ haskellCode "diagram" term) ]
                   [ text "Haskell" ]
          ])
      ]
    ]
    , h2_ [ text "Term view" ]
    , footer_ $ inferredType # either (const []) (\{ term } -> [slot _term unit Term.termView { term, selection: selectionPath } \_ -> Nothing])
    , a [ href "https://statebox.org" ] []
  ]
  where
    bricksInput :: Bricks.Input
    bricksInput = toBricksInput st.input st.selectionBox

    envE :: String \/ Context String String
    envE = (<>) <$> String.parseContext st.input.context <*> pure defaultEnv

    inferredType = envE <#> \env -> inferType env bricksInput.bricks.term
    termTypeStr = inferredType # either identity showInferred
    selectionPath = Bricks.toSelection st.selectionBox bricksInput.bricks.term Nil
    selectionType = hush inferredType <#> \{ errors, term } -> showInferred { errors, term: getSubTerm selectionPath term }

toBricksInput :: Input -> Box -> Bricks.Input
toBricksInput input selectionBox =
  { bricks, matches, context, selectedBoxes, renderBoxContent: Bricks.defaultRenderBoxContent }
  where
    bricks = Bricks.fromPixels (String.parsePixels input.pixels) (\s -> s == " " || s == "-" || s == "=")

    context = envE # either (const defaultEnv) identity
    envE = (<>) <$> String.parseContext input.context <*> pure defaultEnv

    inferredType = envE <#> \env -> inferType env bricks.term
    matches = inferredType # either (\envError -> []) _.matches

    selectionPath = Bricks.toSelection selectionBox bricks.term Nil
    selectedBoxes = either (const Set.empty) (getSubTerm selectionPath >>> foldMap f) (inferredType <#> _.term) where
      f { box, bid } = Set.singleton { box, bid }

handleInput :: ∀ o m. MonadEffect m => Input -> H.HalogenM State Action ChildSlots o m Unit
handleInput input = do
  st <- H.modify $ set (_input) input
  updateWindowLocation st.input

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
    for_ (escape pixels) \p ->
      for_ (escape context) \c ->
        setHash ("pixels=" <> p <> "&context=" <> c) l

escape :: String -> Maybe String
escape s = encodeURIComponent s
  <#> replaceAll (Pattern "(") (Replacement "%28")
  <#> replaceAll (Pattern ")") (Replacement "%29")
