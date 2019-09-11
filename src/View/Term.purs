module View.Term where

import Prelude hiding (div)

import Data.Either (either, hush)
import Data.Foldable (foldMap, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), drop)
import Data.Maybe
import Data.String.Pattern (Pattern(..))
import Data.String.Common (trim, split)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Properties (classes, value, readOnly)
import Halogen.HTML.Events (onValueInput)

import Debug.Trace

import Model
import Common (VoidF)


type State = 
  { term :: Term Ann (Brick String)
  , selectionPath :: Path /\ Int
  }

type Input = State

data Action =
  Update State

type Slot = H.Slot VoidF Void


termView :: ∀ q m. H.Component HTML q Input Void m
termView =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Update }
    }

initialState :: Input -> State
initialState st@{ term } = trace (json term) \_ -> st

handleAction :: ∀ o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Update st@{ term } -> do
    traceM (json term)
    H.put st

render :: ∀ m. State -> H.ComponentHTML Action () m
render { term, selectionPath: path /\ count } = div [ classes [ ClassName "term" ] ] (rec (Just path) term) where
  rec :: Maybe Path -> Term Ann (Brick String) -> Array (H.ComponentHTML Action () m)
  rec p TUnit = [ div [ clsSel p "tunit" ] [ i_ [ text "I" ] ] ]
  rec p (TBox { bid }) = [ div [ clsSel p "tbox" ] [ format bid ] ]
  rec p (TC terms _) = [ div [ clsSel p "tc" ] $ intercalate [ div_ [ text "⊙" ] ] $ withPath rec p terms ]
  rec p (TT terms _) = [ div [ clsSel p "tt" ] $ intercalate [ div_ [ text "⊗" ] ] $ withPath rec p terms ]
  clsSel (Just Nil) n = classes [ ClassName n, ClassName "selected" ] 
  clsSel _ n = classes [ ClassName n ] 
  withPath :: ∀ a b. (Maybe Path -> a -> b) -> Maybe Path -> Array a -> Array b
  withPath rec Nothing = map (rec Nothing)
  withPath rec (Just Nil) = map (rec Nothing)
  withPath rec (Just (Cons i Nil)) = mapWithIndex (\j -> rec (if i <= j && j < i + count then Just Nil else Nothing))
  withPath rec (Just (Cons i p)) = mapWithIndex (\j -> rec (if i == j then Just p else Nothing))

format :: ∀ m. String -> H.ComponentHTML Action () m
format " " = i_ [ text "I" ]
format "0" = i_ [ text "I" ]
format "-" = i_ [ text "id" ]
format "=" = i_ [ text "id" ]
format bid = span_ [ text bid ]

json :: Term Ann (Brick String) -> String
json TUnit = """{ "type": "unit" }"""
json (TBox { bid }) = """{ "type": "morphism", "name": """ <> "\"" <> bid <> "\"" <> """" }"""
json (TC terms _) = """{ "type": "compose", "terms": [""" <> intercalate ", " (map json terms) <> """] }"""
json (TT terms _) = """{ "type": "tensor", "terms": [""" <> intercalate ", " (map json terms) <> """] }"""