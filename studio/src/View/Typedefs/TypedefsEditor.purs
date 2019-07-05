module View.Typedefs.TypedefsEditor where

import Prelude

import Data.Foldable (foldMap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typedef.Typedef2
import Halogen.HTML (HTML, text, div, pre, table, tr, td)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..))

typedefsTreeView :: ∀ a b. Array (TypeName /\ Typedef2) -> HTML a b
typedefsTreeView tdefs =
  table []
        [ tr [ classes [ ClassName "css-typedef-list" ] ]
             (uncurry namedTypedefHtml <$> tdefs)
        ]

namedTypedefHtml :: ∀ a b. TypeName -> Typedef2 -> HTML a b
namedTypedefHtml name tdef =
  tr []
     [ td [ classes [ ClassName "css-typedef-name" ] ]
          [ text name ]
     , td [ classes [ ClassName "css-typedef-equals" ] ]
          [ text " =" ]
     , td []
          [ typedefHtml tdef ]
     ]

typedefHtml :: ∀ a b. Typedef2 -> HTML a b
typedefHtml = case _ of
  TUnit       -> leafType "1"
  TRef  n     -> leafType n
  TSum  tdefs -> accoladedType Plus  (typedefHtml <$> tdefs)
  TProd tdefs -> accoladedType Times (typedefHtml <$> tdefs)
  where
    accoladedType :: OperatorKind -> Array (HTML a b) -> HTML a b
    accoladedType op htmls =
      div [ classes [ ClassName "css-typedef-definition" ] ]
          [ div [ classes [ ClassName "cols1" ] ]
                [ operatorColumn op
                , accoladeColumn op
                , subtermsColumn htmls
                ]
          ]
      where
        operatorColumn op =
          div [ classes [ ClassName "col", ClassName "col0" ] ]
              [ div [ classes [ ClassName "operator-symbol-container" ] ]
                    [ text " " ]
              ]

        accoladeColumn operator =
          div [ classes [ ClassName "col", ClassName "col1", operatorClass ] ]
              [ div [ classes [ ClassName "row1" ] ]
                    [ div [ classes [ ClassName "mid1" ] ] [ text operatorText ]
                    , div [ classes [ ClassName "top1" ] ] [ text " " ]
                    , div [ classes [ ClassName "bot1" ] ] [ text " " ]
                    ]
              ]
          where
            operatorText = case operator of
              Plus  -> "+"
              Times -> "×"
              Mu    -> "&mu;"

            operatorClass = ClassName $ case operator of
              Plus  -> "css-sum"
              Times -> "css-product"
              Mu    -> "css-mu"

        subtermsColumn :: Array (HTML a b) -> HTML a b
        subtermsColumn htmls =
          div [ classes [ ClassName "col", ClassName "col2" ] ]
              htmls

leafType str =
  div [ classes [ ClassName "css-leaf-type" ] ]
      [ text str ]
