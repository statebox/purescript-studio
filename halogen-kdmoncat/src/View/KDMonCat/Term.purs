module View.KDMonCat.Term where

import Prelude hiding (div)

import Data.Array (fromFoldable, head, take, length)
import Data.Foldable (intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML hiding (map, head)
import Halogen.HTML.Properties (classes)

import View.ReactiveInput as ReactiveInput
import KDMonCat.Model
import KDMonCat.Common (VoidF, Fix(..), Ann(..))

type TTerm = TypedTerm String String

type Input =
  { term :: TTerm
  , selection :: Selection
  }

type Slot = H.Slot VoidF Void


termView :: ∀ q m. MonadEffect m => H.Component HTML q Input Void m
termView =
  ReactiveInput.mkComponent
    { render
    , initialState : {}
    , handleAction : \_ _ -> pure unit
    , handleInput  : \_ -> pure unit
    }

render :: ∀ m. Input -> {} -> H.ComponentHTML Void () m
render { term, selection: { path, count } } _ = div [ classes [ ClassName "kdmoncat-term" ] ] (rec (Just path) term) where
  rec :: Maybe Path -> TTerm -> Array (H.ComponentHTML Void () m)
  rec p (Fix (Ann _ TUnit)) = [ div [ clsSel p "tunit" ] [ i_ [ text "I" ] ] ]
  rec p (Fix (Ann ty (TBox { name, decl }))) = [ div [ clsSel p "tbox" ] [ format ty decl name ] ]
  rec p (Fix (Ann _ (TC terms))) = [ div [ clsSel p "tc" ] $ intercalate [ div_ [ text "⊙" ] ] $ withPath p terms ]
  rec p (Fix (Ann _ (TT terms))) = [ div [ clsSel p "tt" ] $ intercalate [ div_ [ text "⊗" ] ] $ withPath p terms ]
  clsSel (Just Nil) n = classes [ ClassName n, ClassName "selected" ]
  clsSel _ n = classes [ ClassName n ]
  withPath Nothing = map (rec Nothing)
  withPath (Just Nil) = map (rec Nothing)
  withPath (Just (Cons i Nil)) = mapWithIndex (\j -> rec (if i <= j && j < i + count then Just Nil else Nothing))
  withPath (Just (Cons i p)) = mapWithIndex (\j -> rec (if i == j then Just p else Nothing))

format :: ∀ m. Ty String -> TypeDecl String -> String -> H.ComponentHTML Void () m
format _ (Perm []) _ = i_ [ text "I" ]
format (Ty vs _) (Perm p) _ | take (length p) [1,2,3,4,5,6,7,8,9] == p = i_ [ text "id", formatTy vs "⊗" ]
format (Ty vs _) (Perm _) name = i_ [ text name, formatTy vs "," ]
format (Ty l r) (Spider _ _ _) name = i_ [ text name, formatTy (l <> r # head # fromFoldable) "" ]
format _ _ name = span_ [ text name ]

formatTy :: ∀ m. Array String -> String -> H.ComponentHTML Void () m
formatTy vs sep = sub_ [ text (intercalate sep vs) ]
