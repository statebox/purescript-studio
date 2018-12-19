module View.Studio where

import Prelude hiding (div)
import Data.Array (catMaybes)
import Data.Either.Nested (Either2)
import Data.Foldable (find, foldMap)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text, ul, li, aside, span, i)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA

import View.Model (Project, ProjectName, mkNetInfoWithTypesAndRoles)
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRoles, emptyNetInfo, NetObj, QueryF(..), Msg(NetUpdated))
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Studio.ObjectTree as ObjectTree
import View.Auth.RolesEditor as RolesEditor
import View.Studio.Route (Route, RouteF(..), routesObjNameEq)
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

type State =
  { route      :: Route
  , projects   :: Array Project
  , msg        :: String
  }

--------------------------------------------------------------------------------

data Query a
  = SelectRoute Route a
  | HandlePetrinetEditorMsg Msg a
  | HandleDiagramEditorMsg Unit a

type ChildQuery = Coproduct2 (PetrinetEditor.QueryF PID TID) DiagramEditor.Query

type ChildSlot = Either2 Unit Unit

petrinetEditorSlotPath = ChildPath.cp1
diagramEditorSlotPath = ChildPath.cp2

--------------------------------------------------------------------------------

ui :: forall m. MonadAff m => H.Component HTML Query Unit Void m
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState =
      { msg:        "Welcome to Statebox Studio!"
      , projects:   Ex.projects
      , route:      Home
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      HandlePetrinetEditorMsg NetUpdated next -> do
        -- TODO
        pure next

      SelectRoute route next -> do
        case route of
          Home -> do
            H.modify_ (\state -> state { route = Home })
            pure next
          Types projectName -> do
            H.modify_ (\state -> state { route = Types projectName })
            pure next
          Auths projectName -> do
            H.modify_ (\state -> state { route = Auths projectName })
            pure next
          Net projectName netInfo -> do
            state <- H.get
            H.put $ state { route = Net projectName netInfo }
            let netInfoWithTypesAndRolesMaybe = mkNetInfoWithTypesAndRoles netInfo <$> findProject state.projects projectName
            _ <- (H.query' petrinetEditorSlotPath unit <<< H.action <<< LoadNet) `traverse` netInfoWithTypesAndRolesMaybe
            pure next
          r@(Diagram projectName diagramInfo) -> do
            H.modify_ (\state -> state { route = r })
            pure next

      HandleDiagramEditorMsg unit next -> do
        pure next

    render :: State -> ParentHTML Query ChildQuery ChildSlot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "columns" ] ]
              [ div [ classes [ ClassName "column", ClassName "is-narrow" ] ]
                    [ ObjectTree.menu SelectRoute (routesObjNameEq state.route) state.projects ]
              , div [ classes [ ClassName "column" ] ]
                    [ routeBreadcrumbs
                    , maybe (text "TODO project not found") mainView (f1 state.route)
                    ]
              ]
        ]
      where
        mainView :: RouteF Project -> ParentHTML Query ChildQuery ChildSlot m
        mainView route = case route of
          Home ->
            text "Please select an object from the menu, such as a Petri net or a diagram."
          Types project ->
            TypedefsEditor.typedefsTreeView project.types
          Auths project ->
            RolesEditor.roleInfosHtml project.roleInfos
          Net project netInfo ->
            HH.slot' petrinetEditorSlotPath unit (PetrinetEditor.ui (mkNetInfoWithTypesAndRoles netInfo project)) unit (HE.input HandlePetrinetEditorMsg)
          Diagram project diagramInfo ->
            HH.slot' diagramEditorSlotPath unit DiagramEditor.ui unit (HE.input HandleDiagramEditorMsg)

        routeBreadcrumbs :: ParentHTML Query ChildQuery ChildSlot m
        routeBreadcrumbs =
          nav [ classes [ ClassName "breadcrumb has-arrow-separator", ClassName "is-small" ]
              , ARIA.label "breadcrumbs"
              ]
              [ ul [] $ crumb <$> case state.route of
                                    Home                         -> [ "Home" ]
                                    Types   projectName          -> [ projectName, "Types" ]
                                    Auths   projectName          -> [ projectName, "Authorisation" ]
                                    Net     projectName { name } -> [ projectName, name ]
                                    Diagram projectName { name } -> [ projectName, name ]
              ]
          where
            crumb str = li [] [ a [ href "" ] [ text str ] ]

        navBar :: ParentHTML Query ChildQuery ChildSlot m
        navBar =
          nav [ classes [ ClassName "navbar" ] ]
              [ div [ classes [ ClassName "navbar-brand" ] ]
                    [ a [ classes [ ClassName "navbar-item" ] ]
                        [ img [ src "logo-statebox.jpg" ]
                        ]
                    ]
              , div [ classes [ ClassName "navbar-menu" ] ]
                    [ div [ classes [ ClassName "navbar-start" ] ]
                          [ div [ classes [ ClassName "navbar-item" ] ]
                                [ h1 [ classes [ ClassName "subtitle" ] ] [ text "Statebox Studio" ] ]
                          ]
                    , div [ classes [ ClassName "navbar-end" ] ]
                          [ a   [ classes [ ClassName "navbar-item" ] ] [ text "Development" ] ]
                    ]
              ]

        f1 :: RouteF ProjectName -> Maybe (RouteF Project)
        f1 = case _ of
          Net     projectName netInfo     -> flip Net netInfo         <$> findProject state.projects projectName
          Types   projectName             -> Types                    <$> findProject state.projects projectName
          Auths   projectName             -> Auths                    <$> findProject state.projects projectName
          Diagram projectName diagramInfo -> flip Diagram diagramInfo <$> findProject state.projects projectName
          Home                            -> pure Home

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects
