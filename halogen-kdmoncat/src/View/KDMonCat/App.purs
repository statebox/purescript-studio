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
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Global (encodeURIComponent)
import Halogen as H
import Halogen.HTML hiding (map, head, i, prop, title)
import Halogen.HTML.Properties (classes, value, readOnly, href, type_, InputType(InputText), ref, title)
import Halogen.HTML.Events (onValueInput, onClick, onMouseDown)
import Halogen.Query.Input (RefLabel(..))
import View.ReactiveInput as ReactiveInput
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

import KDMonCat.Bricks as Bricks
import KDMonCat.InferType
import KDMonCat.Model

import KDMonCat.Input.String as String
import KDMonCat.Output.Haskell (haskellCode)
import KDMonCat.Output.JSON (json)

import View.KDMonCat.Bricks as Bricks
import View.KDMonCat.Term as Term
import View.KDMonCat.EditDomHelpers (copyToClipboard, insertText)

type Input = String.Input
type Output = String.Input

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


type Slot = H.Slot Query Output

data Action
  = UpdatePixels String
  | UpdateContext String
  | BricksMessage Bricks.Output
  | CopyToClipboard String
  | InsertPixel String
  | PreventDefault Action Event

data Query a
  = DoAction Action a

type ChildSlots =
  ( bricks :: Bricks.Slot Unit
  , term :: Term.Slot Unit
  )

_bricks = SProxy :: SProxy "bricks"
_term = SProxy :: SProxy "term"


appView :: ∀ m. MonadEffect m => H.Component HTML Query Input Output m
appView =
  ReactiveInput.mkComponentWithQuery
    { initialState
    , render
    , handleAction
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
      , textarea [ value st.input.pixels, onValueInput (Just <<< UpdatePixels), ref (RefLabel "pixels") ]
      , div_ pixelButtons
      , h2_ [ text "Context"]
      , textarea [ value st.input.context, onValueInput (Just <<< UpdateContext), ref (RefLabel "context") ]
      , div_ [ text "F.e.: ", code_ [ text "a b -> c d, [4 2 3 1] (Wire swaps), 3.5, 4o2 (Spiders)" ] ]
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

    predefined =
      [ "-" /\ "id, type [1]"
      , "=" /\ "id ⊗ id, type [1 2]"
      , "σ" /\ "Swap, type [2 1]"
      , "ε" /\ "Counit (discard), type 1.0"
      , "δ" /\ "Comultiply (copy), type 1.2"
      , "η" /\ "Unit (zero), type 1o0"
      , "μ" /\ "Multiply (sum), type 1o2"
      , "(" /\ "Cup"
      , ")" /\ "Cap"
      ]
    pixelButtons = predefined <#> \(pixel /\ help) ->
      button [ onMouseDown $ Just <<< PreventDefault (InsertPixel pixel) <<< toEvent
             , title help ]
             [ text pixel ]

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

handleInput :: ∀ m. MonadEffect m => Input -> H.HalogenM State Action ChildSlots Output m Unit
handleInput input = do
  H.put $ initialState { input = input }
  updateWindowLocation input

handleAction :: ∀ m. MonadEffect m => Input -> Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction inp = case _ of
  UpdatePixels p -> do
    st <- H.modify $ set (_input <<< _pixels) p
    H.raise st.input
    updateWindowLocation st.input
  UpdateContext c -> do
    st <- H.modify $ set (_input <<< _context) c
    H.raise st.input
    updateWindowLocation st.input
  BricksMessage (Bricks.SelectionChanged sel) ->
    H.modify_ $ set _selectionBox sel
  CopyToClipboard s ->
    liftEffect (copyToClipboard s)
  InsertPixel s ->
    liftEffect (insertText s)
  PreventDefault action event -> do
    H.liftEffect $ Event.preventDefault event
    handleAction inp action

handleQuery :: ∀ m a. MonadEffect m => Input -> Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
handleQuery inp (DoAction x next) = do
  handleAction inp x
  pure (Just next)

updateWindowLocation :: ∀ m. MonadEffect m => Input -> H.HalogenM State Action ChildSlots Output m Unit
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
