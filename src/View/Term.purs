module View.Term where

import Prelude hiding (div)

import Data.Array (fromFoldable, head)
import Data.Foldable (intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML hiding (map, head)
import Halogen.HTML.Properties (classes)

import Model
import Common (VoidF, Fix(..), Ann(..))

type TTerm = TypedTerm String String

type State =
  { term :: TTerm
  , selection :: Selection
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
initialState st@{ term } = st

handleAction :: ∀ o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Update st@{ term } -> do
    H.put st

render :: ∀ m. State -> H.ComponentHTML Action () m
render { term, selection: { path, count } } = div [ classes [ ClassName "term" ] ] (rec (Just path) term) where
  rec :: Maybe Path -> TTerm -> Array (H.ComponentHTML Action () m)
  rec p (Fix (Ann _ TUnit)) = [ div [ clsSel p "tunit" ] [ i_ [ text "I" ] ] ]
  rec p (Fix (Ann ty (TBox { bid, decl }))) = [ div [ clsSel p "tbox" ] [ format ty decl bid ] ]
  rec p (Fix (Ann _ (TC terms))) = [ div [ clsSel p "tc" ] $ intercalate [ div_ [ text "⊙" ] ] $ withPath p terms ]
  rec p (Fix (Ann _ (TT terms))) = [ div [ clsSel p "tt" ] $ intercalate [ div_ [ text "⊗" ] ] $ withPath p terms ]
  clsSel (Just Nil) n = classes [ ClassName n, ClassName "selected" ]
  clsSel _ n = classes [ ClassName n ]
  withPath Nothing = map (rec Nothing)
  withPath (Just Nil) = map (rec Nothing)
  withPath (Just (Cons i Nil)) = mapWithIndex (\j -> rec (if i <= j && j < i + count then Just Nil else Nothing))
  withPath (Just (Cons i p)) = mapWithIndex (\j -> rec (if i == j then Just p else Nothing))

format :: ∀ m. Ty (VarWithBox String) -> TypeDecl String -> String -> H.ComponentHTML Action () m
format _ (Perm []) _ = i_ [ text "I" ]
format (Ty vs _) (Perm [1]) _ = i_ [ text "id", formatTy vs "⊗" ]
format (Ty vs _) (Perm [1, 2]) _ = i_ [ text "id", formatTy vs "⊗" ]
format (Ty vs _) (Perm _) bid = i_ [ text bid, formatTy vs "," ]
format (Ty l r) (Spider _ _ _) bid = i_ [ text bid, formatTy (l <> r # head # fromFoldable) "" ]
format _ _ bid = span_ [ text bid ]

formatTy :: ∀ m. Array (VarWithBox String) -> String -> H.ComponentHTML Action () m
formatTy vs sep = sub_ [ text (intercalate sep (vs <#> _.var >>> show)) ]
