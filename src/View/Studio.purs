module View.Studio where

import Prelude hiding (div)
import Affjax as Affjax
import Data.Array (catMaybes, head)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Either (Either(..), either)
import Data.Either.Nested (Either3)
import Data.Foldable (find, foldMap)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text, ul, ol, li, aside, span, i, br, pre)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes, src, href, placeholder)
import Halogen.HTML.Properties.ARIA as ARIA

import Statebox.API (shortHash, findRootDiagramMaybe)
import Statebox.API.Client as Stbx
import Statebox.API.Client (DecodingError(..))
import Statebox.API.Types (HashStr, URL, WiringTx, Wiring, FiringTx, TxSum(..), Tx, Diagram)
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
import View.Studio.Route (Route, RouteF(..), NetName, DiagramName, NodeIdent(..), NamespaceInfo(..))
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

type State =
  { route       :: Route
  , projects    :: Array Project
  , namespaces  :: Map HashStr NamespaceInfo
  , wirings     :: Map HashStr WiringTx
  , firings     :: Map HashStr FiringTx
  , msg         :: String
  }

--------------------------------------------------------------------------------

data Query a
  = SelectRoute Route a
  | LoadFromHash URL HashStr a
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
      , namespaces:  mempty
      , wirings:     mempty
      , firings:     mempty
      , route:       Home
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      HandleObjectTreeMsg (ObjectTree.Clicked pathId route) next -> do
        eval (SelectRoute route next)

      SelectRoute route next -> do
        H.modify_ \state -> state { route = route }
        pure next

      LoadFromHash endpointUrl hash next -> do
        H.liftEffect $ log $ "LoadFromHash: requesting transaction " <> hash <> " from " <> endpointUrl
        res <- H.liftAff $ Stbx.requestTransaction endpointUrl hash
        res # either
          (\err   -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (either (\(DecodingError err) -> H.liftEffect $ log $ "Expected to decode a wiring or firing: " <> show err)
                  (\wf                  -> case wf of
                                             LeInitial hash -> do
                                               H.liftEffect $ log $ "genesis transaction at hash " <> hash
                                               let namespace = { name: shortHash hash, hash: hash }
                                               H.modify_ (\state -> state { namespaces = Map.insert hash namespace state.namespaces })
                                             LeWiring wiring -> do
                                               H.liftEffect $ log $ "wiring: " <> show wiring
                                               H.modify_ (\state -> state { wirings = Map.insert hash wiring state.wirings })
                                             LeFiring firing -> do
                                               H.liftEffect $ log $ "firing: " <> show firing
                                               H.modify_ (\state -> state { firings = Map.insert hash firing state.firings })
                  )
          )
        pure next

      HandleDiagramEditorMsg (DiagramEditor.OperatorClicked opId) next -> do
        H.liftEffect $ log $ "DiagramEditor.OperatorClicked: " <> opId
        state <- H.get
        let
          -- TODO #87 we hardcode the assumption here that opId is a net (NetNode opId) but it could be (LeDiagram opId)
          newRouteMaybe :: Maybe Route
          newRouteMaybe = case state.route of
            Diagram pname dname _ -> Just (Diagram pname dname (Just (NetNode opId)))
            _                     -> Nothing
        maybe (pure next) (\route -> eval (SelectRoute route next)) newRouteMaybe

      HandlePetrinetEditorMsg NetUpdated next -> do
        pure next

    render :: State -> ParentHTML Query ChildQuery ChildSlot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "flex" ] ]
              [ div [ classes [ ClassName "w-1/6", ClassName "h-12" ] ]
                    [ HH.slot' objectTreeSlotPath unit (ObjectTree.menuComponent (_ == state.route)) (projectsToTree state) (HE.input HandleObjectTreeMsg) ]
              , div [ classes [ ClassName "w-5/6", ClassName "h-12" ] ]
                    [ routeBreadcrumbs
                    , maybe (text "Couldn't find project/net/diagram.") mainView (reifyRoute state.projects state.route)
                    ]
              ]
        ]
      where
        mainView :: RouteF Project DiagramInfo NetInfoWithTypesAndRoles -> ParentHTML Query ChildQuery ChildSlot m
        mainView route = case route of
          Home ->
            div []
                [ text "Please select an object from the menu, or enter a transaction hash below."
                , br [], br []
                , HH.input [ HP.value ""
                           , placeholder "Enter transaction hash"
                           , HE.onValueInput $ HE.input (LoadFromHash Ex.endpointUrl)
                           , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
                           ]
                ]
          Types project ->
            TypedefsEditor.typedefsTreeView project.types
          Auths project ->
            RolesEditor.roleInfosHtml project.roleInfos
          Net project netInfo ->
            HH.slot' petrinetEditorSlotPath unit PetrinetEditor.ui netInfo (HE.input HandlePetrinetEditorMsg)
          Diagram project diagramInfo nodeMaybe ->
            div [ classes [ ClassName "flex" ] ]
                [ div [ classes [ ClassName "w-1/2" ] ]
                      [ HH.slot' diagramEditorSlotPath unit DiagramEditor.ui diagramInfo.ops (HE.input HandleDiagramEditorMsg) ]
                , div [ classes [ ClassName "w-1/2", ClassName "pl-4" ] ]
                      [ case nodeMaybe of
                          Just (NetNode netInfo)          -> HH.slot' petrinetEditorSlotPath unit PetrinetEditor.ui netInfo (HE.input HandlePetrinetEditorMsg)
                          Just (DiagramNode diagramInfo2) -> text "TODO viewing internal diagrams is not supported yet."
                          Nothing                         -> text "Click a node to show the corresponding net or diagram."
                      ]
                ]
          NamespaceR hash ->
            text $ "Namespace " <> hash
          WiringR wfi ->
            let
              wiringMaybe :: Maybe WiringTx
              wiringMaybe = wfi.hash `Map.lookup` state.wirings

              rootDiagramMaybe :: Maybe Diagram
              rootDiagramMaybe = findRootDiagramMaybe =<< wiringMaybe

              diagramInfoMaybe :: Maybe DiagramInfo
              diagramInfoMaybe = (\d -> { name: d.name, ops: [] }) <$> rootDiagramMaybe
            in
            div []
                [ text $ "Wiring " <> wfi.hash <> " at " <> wfi.endpointUrl <> "."
                , br [], br []
                , maybe (text "wiring not found")
                        (\w -> pre [] [ text $ show w ])
                        wiringMaybe
                , br [], br []
                , text $ "rootDiagramMaybe: " <> show rootDiagramMaybe
                , br [], br []
                , text $ "diagramInfoMaybe: " <> show diagramInfoMaybe
                ]

          FiringR x ->
            text $ "Firing " <> x.hash <> " at " <> x.endpointUrl <> "."

        routeBreadcrumbs :: ParentHTML Query ChildQuery ChildSlot m
        routeBreadcrumbs =
          nav [ classes $ ClassName <$> [ "css-route-breadcrumbs", "rounded", "font-sans", "w-full", "mt-4", "mb-4" ] ]
              [ ol [ classes $ ClassName <$> [ "list-reset", "flex", "text-grey-dark" ] ] $
                   crumb <$> case state.route of
                               Home                          -> [ "Home" ]
                               Types      projectName        -> [ projectName, "Types" ]
                               Auths      projectName        -> [ projectName, "Authorisation" ]
                               Net        projectName name   -> [ projectName, name ]
                               Diagram    projectName name _ -> [ projectName, name ]
                               NamespaceR hash               -> [ "namespace", shortHash hash ]
                               WiringR    x                  -> [ x.endpointUrl, "wiring " <> shortHash x.hash ]
                               FiringR    x                  -> [ x.endpointUrl, shortHash x.hash ]
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
              , menu [ "Home"    /\ Just Home
                     , "Project" /\ Nothing
                     , "Help"    /\ Nothing
                     ]
              ]
          where
            menu items =
              div [ classes $ ClassName <$> ["w-full", "block", "flex-grow", "lg:flex", "lg:items-center", "lg:w-auto" ] ]
                  [ div [ classes $ ClassName <$> ["text-sm", "lg:flex-grow" ] ]
                        (menuItem <$> items)
                  ]

            menuItem (label /\ routeMaybe) =
              a (
                  [ classes $ ClassName <$> [ "block", "mt-4", "lg:inline-block", "lg:mt-0", "text-purple-lighter", "hover:text-white", "mr-4" ]
                  , href "#"
                  ]
                  <> ((\r -> [ HE.onClick $ HE.input_ (SelectRoute r) ]) `foldMap` routeMaybe)
                )
                [ text label ]

--------------------------------------------------------------------------------

reifyRoute :: Array Project -> RouteF ProjectName DiagramName NetName -> Maybe (RouteF Project DiagramInfo NetInfoWithTypesAndRoles)
reifyRoute projects = case _ of
  Home                              -> pure Home
  Types     projectName             -> Types <$> findProject projects projectName
  Auths     projectName             -> Auths <$> findProject projects projectName
  Net       projectName name        -> do project <- findProject projects projectName
                                          net     <- findNetInfoWithTypesAndRoles project name
                                          pure $ Net project net
  Diagram   projectName name nodeId -> do project <- findProject projects projectName
                                          diagram <- findDiagramInfo project name
                                          let node = nodeId >>= case _ of
                                                       DiagramNode dn -> DiagramNode <$> findDiagramInfo              project dn
                                                       NetNode     nn -> NetNode     <$> findNetInfoWithTypesAndRoles project nn
                                          pure $ Diagram project diagram node
  NamespaceR hash                   -> pure $ NamespaceR hash
  WiringR    x                      -> pure $ WiringR    x
  FiringR    x                      -> pure $ FiringR    x

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects

findNetInfo :: Project -> NetName -> Maybe NetInfo
findNetInfo project netName = find (\n -> n.name == netName) project.nets

findNetInfoWithTypesAndRoles :: Project -> NetName -> Maybe NetInfoWithTypesAndRoles
findNetInfoWithTypesAndRoles project netName = do
  netInfo <- findNetInfo project netName
  pure $ mkNetInfoWithTypesAndRoles netInfo project

findDiagramInfo :: Project -> DiagramName -> Maybe DiagramInfo
findDiagramInfo project diagramName = find (\d -> d.name == diagramName) project.diagrams

--------------------------------------------------------------------------------

projectsToTree :: State -> Cofree Array ObjectTree.Item
projectsToTree { projects, namespaces, wirings } =
  mkItem ["Studio"] "Studio" Nothing
    :< (namespaceItems <> wiringItems <> projectItems)
  where
    namespaceItems = uncurry fromNamespace <$> Map.toUnfoldable namespaces
    wiringItems    = (uncurry fromWiring <<< map _.wiring) <$> Map.toUnfoldable wirings
    projectItems   = fromProject <$> projects

    fromProject :: Project -> Cofree Array ObjectTree.Item
    fromProject p =
      mkItem [p.name] p.name Nothing :<
        [ mkItem [ p.name, "types"          ] "Types"          (Just $ Types p.name) :< []
        , mkItem [ p.name, "authorisations" ] "Authorisations" (Just $ Auths p.name) :< []
        , mkItem [ p.name, "nets"           ] "Protocols"      (Nothing)             :< fromNets     p p.nets
        , mkItem [ p.name, "diagrams "      ] "Diagrams"       (Nothing)             :< fromDiagrams p p.diagrams
        ]
      where
        fromNets     p nets  = (\n -> mkItem [ p.name, "nets",     n.name ] n.name (Just $ Net     p.name n.name        ) :< []) <$> nets
        fromDiagrams p diags = (\d -> mkItem [ p.name, "diagrams", d.name ] d.name (Just $ Diagram p.name d.name Nothing) :< []) <$> diags

    fromNamespace :: HashStr -> NamespaceInfo -> Cofree Array ObjectTree.Item
    fromNamespace hash n =
      mkItem [ "ALL_NAMESPACES", n.hash ]
             ("n " <> n.name)
             (Just $ NamespaceR n.name) :< []

    fromWiring :: HashStr -> Wiring -> Cofree Array ObjectTree.Item
    fromWiring hash w =
      mkItem [ "ALL_WIRINGS", hash ]
             ("w " <> shortHash hash)
             (Just route) :< fromNets     fakeProject w.nets
                          <> fromDiagrams fakeProject w.diagrams
      where
        fakeProject          = { id: hash, name: shortHash hash }
        route                = WiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash }
        fromNets     p nets  = (\n -> mkItem [ p.id, "nets",     n.name ] ("n " <> n.name) (Just $ Net     p.name n.name        ) :< []) <$> nets
        fromDiagrams p diags = (\d -> mkItem [ p.id, "diagrams", d.name ] ("d " <> d.name) (Just $ Diagram p.name d.name Nothing) :< []) <$> diags
