module View.Studio.ObjectTree where

import Prelude hiding (div)
import Effect.Aff.Class (class MonadAff)
import Data.Foldable (foldMap)
import Data.Monoid (guard)
import Halogen (ParentHTML)
import Halogen.HTML (HTML, nav, div, p, a, text, ul, li, aside, span, i)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA

import Data.Auth (RoleInfo)
import View.Model (Project, ProjectName)
import View.Petrinet.Model (NetInfo)
import View.Diagram.Model (DiagramInfo)
import View.Studio.Route (Route, RouteF(..), routesObjNameEq)

componentCssClassName = ClassName "css-object-chooser"

-- TODO should this be in a panel?
menu
  :: forall q cq cs s m a
   . MonadAff m
  => (Route -> Unit -> q Unit) -- ^ turns a route into a query for the caller
  -> (Route -> Boolean)
  -> Array Project
  -> ParentHTML q cq s m
menu routeToQuery isSelected projects =
  aside [ classes [ ClassName "menu", componentCssClassName ] ]
        (projectTree isSelected `foldMap` projects)
  where
    projectTree :: (Route -> Boolean) -> Project -> Array (ParentHTML q cq s m)
    projectTree isSelected project =
      [ p  [ classes [ ClassName "menu-label" ] ] [ text project.name ]
      , ul [ classes [ ClassName "menu-list" ] ]
           [ p  [ classes [ ClassName "menu-label" ]
                , onClick (HE.input_ (routeToQuery $ Types project.name))
                ] [ text "Types" ]
           , p  [ classes [ ClassName "menu-label" ] ] [ text "Petri nets" ]
           , ul [ classes [ ClassName "menu-list" ] ]
                (netItem isSelected <$> project.nets)
           , p  [ classes [ ClassName "menu-label" ] ] [ text "Wiring Diagrams" ]
           , ul [ classes [ ClassName "menu-list" ] ]
                (diagramItem isSelected <$> project.diagrams)
           , p  [ classes [ ClassName "menu-label" ] ] [ text "Roles" ]
           , ul [ classes [ ClassName "menu-list" ] ]
                (roleItem <$> project.roleInfos)
           ]
      ]
      where
        netItem :: (Route -> Boolean) -> NetInfo -> ParentHTML q cq s m
        netItem isSelected netInfo =
          li []
             [ a [ classes [ ClassName $ guard (isSelected $ Net project.name netInfo) "is-active" ]
                 , onClick (HE.input_ (routeToQuery $ Net project.name netInfo))
                 ]
                 [ text netInfo.name ]
             ]

        diagramItem :: (Route -> Boolean) -> DiagramInfo -> ParentHTML q cq s m
        diagramItem isSelected d =
          li []
             [ a [ classes [ ClassName $ guard (isSelected $ Diagram project.name d) "is-active" ]
                 , onClick (HE.input_ (routeToQuery $ Diagram project.name d))
                 ]
                 [ text d.name ]
             ]

        roleItem :: RoleInfo -> ParentHTML q cq s m
        roleItem roleInfo =
          li []
             [ a [ classes []
                 ]
                 [ text roleInfo.name ]
             ]
