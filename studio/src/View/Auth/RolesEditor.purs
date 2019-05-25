module View.Auth.RolesEditor where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.Newtype (un)
import Halogen.HTML (HTML, div, text, table, thead, tbody, tr, th, td, pre)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..))

import Data.Auth

roleInfosHtml :: ∀ a b. Array RoleInfo -> HTML a b
roleInfosHtml roleInfos =
  table [ classes [ ClassName "css-role-infos", ClassName "table", ClassName "is-striped", ClassName "is-narrow", ClassName "is-hoverable" ] ]
        [ thead []
                [ tr [] [ th [] [ text "id" ]
                        , th [] [ text "name" ]
                        , th [] [ text "background color" ]
                        , th [] [ text "text color" ]
                        ]
                ]
        , tbody [] (roleInfoHtml <$> roleInfos)
        ]
  where
    roleInfoHtml r =
      tr [] [ td []
                 [ text $ show r.id]
            , td []
                 [ text r.name ]
            , td [ classes [ ClassName "css-color-picker" ] ]
                 [ text $ un CSSColor r.bgColor ]
            , td [ classes [ ClassName "css-color-picker" ] ]
                 [ text $ un CSSColor r.textColor ]
            ]
