module View.Studio where

import Prelude hiding (div)
import Data.Array (catMaybes)
import Control.Comonad.Cofree
import Data.Either.Nested (Either3)
import Data.Foldable (find, foldMap)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text, ul, ol, li, aside, span, i)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA

import View.Model (Project, ProjectName, mkNetInfoWithTypesAndRoles)
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRoles, NetObj, QueryF(..), Msg(NetUpdated))
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Studio.ObjectTree as ObjectTree
import View.Studio.ObjectTree (mkItem)
import View.Auth.RolesEditor as RolesEditor
import View.Studio.Route (Route, RouteF(..), routesObjNameEq, NetName, DiagramName)
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

type State =
  { route       :: Route
  , projects    :: Array Project
  , netInfo     :: Maybe NetInfoWithTypesAndRoles
  , diagramInfo :: Maybe DiagramInfo
  , msg         :: String
  }

--------------------------------------------------------------------------------

data Query a
  = SelectRoute Route a
  | HandleObjectTreeMsg ObjectTree.Msg a
  | HandlePetrinetEditorMsg Msg a
  | HandleDiagramEditorMsg DiagramEditor.Msg a

type ChildQuery = Coproduct3 (ObjectTree.Query) (PetrinetEditor.QueryF PID TID) DiagramEditor.Query

type ChildSlot = Either3 Unit Unit Unit

objectTreeSlotPath     = ChildPath.cp1
petrinetEditorSlotPath = ChildPath.cp2
diagramEditorSlotPath  = ChildPath.cp3

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
      { msg:         "Welcome to Statebox Studio!"
      , projects:    Ex.projects
      , route:       Home
      , netInfo:     Nothing
      , diagramInfo: Nothing
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      HandleObjectTreeMsg (ObjectTree.Clicked pathId route) next -> do
        eval (SelectRoute route next)

      HandlePetrinetEditorMsg NetUpdated next -> do
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
          r@(Net projectName netName) -> do
            state <- H.get
            let
              netInfoWithTypesAndRolesMaybe :: Maybe NetInfoWithTypesAndRoles
              netInfoWithTypesAndRolesMaybe = do
                project <- findProject state.projects projectName
                netInfo <- findNetInfo project netName
                pure $ mkNetInfoWithTypesAndRoles netInfo project

            _ <- (H.query' petrinetEditorSlotPath unit <<< H.action <<< LoadNet) `traverse` netInfoWithTypesAndRolesMaybe
            H.put $ state { route = r, netInfo = netInfoWithTypesAndRolesMaybe }
            pure next
          r@(Diagram projectName diagramName) -> do
            state <- H.get
            let
              diagramInfo :: Maybe DiagramInfo
              diagramInfo = do
                project     <- findProject state.projects projectName
                findDiagramInfo project diagramName

            H.put $ state { route = r, diagramInfo = diagramInfo }
            pure next

      HandleDiagramEditorMsg (DiagramEditor.OperatorClicked opId) next -> do
        H.liftEffect $ log $ "DiagramEditor.OperatorClicked: " <> opId
        pure next

    render :: State -> ParentHTML Query ChildQuery ChildSlot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "flex" ] ]
              [ div [ classes [ ClassName "w-1/6", ClassName "h-12" ] ]
                    [ HH.slot' objectTreeSlotPath unit (ObjectTree.menuComponent (routesObjNameEq state.route)) (projectsToTree state.projects) (HE.input HandleObjectTreeMsg) ]
              , div [ classes [ ClassName "w-5/6", ClassName "h-12" ] ]
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
          Net project netName ->
            maybe (text "No net selected.") (\netInfo -> HH.slot' petrinetEditorSlotPath unit PetrinetEditor.ui netInfo (HE.input HandlePetrinetEditorMsg)) state.netInfo
          Diagram project diagramName ->
            maybe (text "No diagram selected.") (\diagramInfo -> HH.slot' diagramEditorSlotPath unit DiagramEditor.ui diagramInfo.ops (HE.input HandleDiagramEditorMsg)) state.diagramInfo

        routeBreadcrumbs :: ParentHTML Query ChildQuery ChildSlot m
        routeBreadcrumbs =
          nav [ classes $ ClassName <$> [ "css-route-breadcrumbs", "rounded", "font-sans", "w-full", "mt-4", "mb-4" ] ]
              [ ol [ classes $ ClassName <$> [ "list-reset", "flex", "text-grey-dark" ] ] $
                   crumb <$> case state.route of
                               Home                     -> [ "Home" ]
                               Types   projectName      -> [ projectName, "Types" ]
                               Auths   projectName      -> [ projectName, "Authorisation" ]
                               Net     projectName name -> [ projectName, name ]
                               Diagram projectName name -> [ projectName, name ]
              ]
          where
            crumb str = li [] [ a [ href "#" ] [ text str ] ]

        navBar :: ParentHTML Query ChildQuery ChildSlot m
        navBar =
          nav [ classes $ ClassName <$> [ "css-navbar", "flex", "items-center", "justify-between", "flex-wrap", "bg-purple-darker", "p-6" ] ]
              [ div [ classes $ ClassName <$> [ "flex", "items-center", "flex-no-shrink", "text-white", "mr-6" ] ]
                    [ img [ src "logo-statebox-white.svg"
                          , classes [ ClassName "css-logo-statebox" ]
                          ]
                    , span [ classes $ ClassName <$> [ "navbar-item", "ml-4", "font-semibold", "text-xl" ] ]
                           [ text "Statebox Studio" ]
                    ]
              , menu [ "Project", "Help" ]
              ]
          where
            menu items =
              div [ classes $ ClassName <$> ["w-full", "block", "flex-grow", "lg:flex", "lg:items-center", "lg:w-auto" ] ]
                  [ div [ classes $ ClassName <$> ["text-sm", "lg:flex-grow" ] ]
                        (menuItem <$> items)
                  ]

            menuItem label =
              a [ classes $ ClassName <$> [ "block", "mt-4", "lg:inline-block", "lg:mt-0", "text-purple-lighter", "hover:text-white", "mr-4" ] ]
                [ text label ]

        f1 :: RouteF ProjectName -> Maybe (RouteF Project)
        f1 = case _ of
          Net     projectName netInfo     -> flip Net netInfo         <$> findProject state.projects projectName
          Types   projectName             -> Types                    <$> findProject state.projects projectName
          Auths   projectName             -> Auths                    <$> findProject state.projects projectName
          Diagram projectName diagramInfo -> flip Diagram diagramInfo <$> findProject state.projects projectName
          Home                            -> pure Home

--------------------------------------------------------------------------------

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects

findNetInfo :: Project -> NetName -> Maybe NetInfo
findNetInfo project netName = find (\n -> n.name == netName) project.nets

findDiagramInfo :: Project -> DiagramName -> Maybe DiagramInfo
findDiagramInfo project diagramName = find (\d -> d.name == diagramName) project.diagrams

--------------------------------------------------------------------------------

projectsToTree :: Array Project -> Cofree Array ObjectTree.Item
projectsToTree projects =
  mkItem ["Studio"] "Studio" Nothing :< (projectToTree <$> projects)
  where
    projectToTree :: Project -> Cofree Array ObjectTree.Item
    projectToTree p =
      mkItem [p.name] p.name Nothing :<
        [ mkItem [ p.name, "types"          ] "Types"          (Just $ Types p.name) :< []
        , mkItem [ p.name, "authorisations" ] "Authorisations" (Just $ Auths p.name) :< []
        , mkItem [ p.name, "nets"           ] "Nets"           (Nothing)             :< fromNets     p.nets
        , mkItem [ p.name, "diagrams "      ] "Diagrams"       (Nothing)             :< fromDiagrams p.diagrams
        ]
      where
        fromNets     nets  = (\n -> mkItem [ p.name, "nets",     n.name ] n.name (Just $ Net     p.name n.name) :< []) <$> nets
        fromDiagrams diags = (\d -> mkItem [ p.name, "diagrams", d.name ] d.name (Just $ Diagram p.name d.name) :< []) <$> diags
