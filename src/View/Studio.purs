module View.Studio where

import Prelude hiding (div)
import Affjax as Affjax
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Array (catMaybes, head, index)
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Either (either, hush)
import Data.Either.Nested (type (\/), Either3)
import Data.Foldable (find, foldMap)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (guard)
import Data.Set as Set
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
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
import Record as Record

import Data.Diagram.FromNLL as FromNLL
import Data.Diagram.FromNLL (ErrDiagramEncoding)
import Statebox.API (shortHash, findRootDiagramMaybe)
import Statebox.API.Client as Stbx
import Statebox.API.Client (DecodingError(..))
import Statebox.API.Types as Stbx
import Statebox.API.Types (HashStr, URL, WiringTx, Wiring, FiringTx, Firing, TxSum(..), Tx, Diagram, PathElem)
import View.Auth.RolesEditor as RolesEditor
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Model (Project, ProjectName)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRoles, QueryF(..), Msg(NetUpdated))
import Data.Petrinet.Representation.NLL as Net
import View.Petrinet.Model.NLL as NLL
import View.Studio.ObjectTree as ObjectTree
import View.Studio.ObjectTree (mkItem, MenuTree)
import View.Studio.Route (Route, RouteF(..), ResolvedRouteF(..), NetName, DiagramName, NodeIdent(..), NamespaceInfo(..))
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

type State =
  { route       :: Route
  , projects    :: Array Project
  , namespaces  :: Map HashStr NamespaceInfo
  , wirings     :: Map HashStr WiringTx
  , firings     :: Map HashStr FiringTx
  , hashSpace   :: AdjacencySpace HashStr TxSum -- ^ Hashes and their (tree of) links.
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
      , hashSpace:   AdjacencySpace.empty
      , route:       Home
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      HandleObjectTreeMsg (ObjectTree.Clicked pathId route) next -> do
        eval (SelectRoute route next)

      SelectRoute route next -> do
        -- H.liftEffect $ log $ "route = " <> show route
        H.modify_ \state -> state { route = route }
        pure next


-- TODO print request body (in request function)
-- if body empty (what does that mean, JSON null? empty string? we have a namespace

      LoadFromHash endpointUrl hash next -> do
        H.liftEffect $ log $ "LoadFromHash: requesting transaction " <> hash <> " from " <> endpointUrl
        res <- H.liftAff $ Stbx.requestTransaction endpointUrl hash
        res # either
          (\err   -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (either (\(DecodingError err) -> H.liftEffect $ log $ "Expected to decode a wiring or firing: " <> show err)
                  (\txSum               -> do
                                             H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace hash txSum })
                                             case txSum of
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
                    [ HH.slot' objectTreeSlotPath unit (ObjectTree.menuComponent (_ == state.route)) (stateMenu state) (HE.input HandleObjectTreeMsg) ]
              , div [ classes [ ClassName "w-5/6", ClassName "h-12" ] ]
                    [ routeBreadcrumbs
                    , maybe (text "Couldn't find project/net/diagram.") mainView (resolveRoute state.route state)
                    ]
              ]
        ]
      where
        mainView :: ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles -> ParentHTML Query ChildQuery ChildSlot m
        mainView route = case route of
          ResolvedHome ->
            div []
                [ text "Please select an object from the menu, or enter a transaction hash below."
                , br [], br []
                , HH.input [ HP.value ""
                           , placeholder "Enter transaction hash"
                           , HE.onValueInput $ HE.input (LoadFromHash Ex.endpointUrl)
                           , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
                           ]
                ]
          ResolvedTypes project ->
            TypedefsEditor.typedefsTreeView project.types
          ResolvedAuths project ->
            RolesEditor.roleInfosHtml project.roleInfos
          ResolvedNet netInfo ->
            HH.slot' petrinetEditorSlotPath unit PetrinetEditor.ui netInfo (HE.input HandlePetrinetEditorMsg)
          ResolvedDiagram diagramInfo nodeMaybe ->
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
          ResolvedNamespace hash ->
            text $ "Namespace " <> hash
          ResolvedWiring wfi ->
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
          ResolvedFiring x ->
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
                               DiagramR   hash ix name       -> [ shortHash hash, "diagram " <> show ix <> " " <> name ]
                               NetR       hash ix name       -> [ shortHash hash, "net "     <> show ix <> " " <> name ]
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

resolveRoute :: RouteF ProjectName DiagramName NetName -> State -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
resolveRoute route {projects, wirings} = case route of
  Home                              -> pure ResolvedHome
  Types     projectName             -> ResolvedTypes <$> findProject projects projectName
  Auths     projectName             -> ResolvedAuths <$> findProject projects projectName
  Net       projectName name        -> do project <- findProject projects projectName
                                          net     <- findNetInfoWithTypesAndRoles project name
                                          pure $ ResolvedNet net
  Diagram   projectName name nodeId -> do project <- findProject projects projectName
                                          diagram <- findDiagramInfo project name
                                          let node = nodeId >>= case _ of
                                                       DiagramNode dn -> DiagramNode <$> findDiagramInfo              project dn
                                                       NetNode     nn -> NetNode     <$> findNetInfoWithTypesAndRoles project nn
                                          pure $ ResolvedDiagram diagram node
  NamespaceR hash                   -> pure $ ResolvedNamespace hash
  WiringR    x                      -> pure $ ResolvedWiring x
  FiringR    x                      -> pure $ ResolvedFiring x
  DiagramR   wiringHash ix name     -> (\d -> ResolvedDiagram d Nothing) <$> findDiagramInfoInWirings wirings wiringHash ix name
  NetR       wiringHash ix name     -> (\n -> ResolvedNet     n)         <$> findNetInfoInWirings     wirings wiringHash ix name

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects

findNetInfo :: Project -> NetName -> Maybe NetInfo
findNetInfo project netName = find (\n -> n.name == netName) project.nets

findNetInfoWithTypesAndRoles :: Project -> NetName -> Maybe NetInfoWithTypesAndRoles
findNetInfoWithTypesAndRoles project netName =
  Record.merge { types: project.types, roleInfos: project.roleInfos } <$> findNetInfo project netName

findDiagramInfo :: Project -> DiagramName -> Maybe DiagramInfo
findDiagramInfo project diagramName = find (\d -> d.name == diagramName) project.diagrams

findNetInfoInWirings :: Map HashStr WiringTx -> HashStr -> PathElem -> String -> Maybe NetInfoWithTypesAndRoles
findNetInfoInWirings wirings wiringHash ix name = do
  wiring      <- spy "findNetInfoInWirings: wiring = "  $ Map.lookup wiringHash wirings
  netW        <- spy "findNetInfoInWirings: netW = "    $ wiring.wiring.nets `index` ix
  netTopo     <- spy "findNetInfoInWirings: netTopo = " $ Net.fromNLLMaybe 0 netW.partition
  let
    placeNames = NLL.defaultPlaceNames netTopo
    netInfo    = spy "findNetInfoInWirings: netInfo = " $ NLL.toNetInfoWithDefaults netTopo netW.name placeNames netW.names
  pure $ Record.merge { types: [], roleInfos: [] } netInfo

findDiagramInfoInWirings :: Map HashStr WiringTx -> HashStr -> PathElem -> String -> Maybe DiagramInfo
findDiagramInfoInWirings wirings wiringHash ix name =
  hush =<< diagramEitherMaybe
  where
    diagramEitherMaybe :: Maybe (ErrDiagramEncoding \/ DiagramInfo)
    diagramEitherMaybe = (FromNLL.fromNLL name <<< toNLL) <$> diagramMaybe

    diagramMaybe :: Maybe Diagram
    diagramMaybe = (flip index ix <<< _.wiring.diagrams) =<< wiringMaybe

    wiringMaybe = Map.lookup wiringHash wirings

    toNLL d = [d.width] <> d.pixels

--------------------------------------------------------------------------------

stateMenu :: State -> MenuTree
stateMenu { projects, namespaces, wirings, firings, hashSpace } =
  mkItem "Studio" Nothing :< (txItems <> projectItems)
  where
    txItems        = AdjacencySpace.unsafeToTree transactionMenu hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)
    projectItems   = projectMenu <$> projects

projectMenu :: Project -> MenuTree
projectMenu p =
  mkItem p.name Nothing :<
    [ mkItem "Types"          (Just $ Types p.name) :< []
    , mkItem "Authorisations" (Just $ Auths p.name) :< []
    , mkItem "Nets"           (Nothing)             :< fromNets     p p.nets
    , mkItem "Diagrams"       (Nothing)             :< fromDiagrams p p.diagrams
    ]
  where
    fromNets     p nets  = (\n -> mkItem n.name (Just $ Net     p.name n.name        ) :< []) <$> nets
    fromDiagrams p diags = (\d -> mkItem d.name (Just $ Diagram p.name d.name Nothing) :< []) <$> diags

transactionMenu :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe TxSum -> Array MenuTree -> MenuTree
transactionMenu t hash valueMaybe itemKids =
  maybe (mkItem ("👻 " <> shortHash hash) unloadedRoute :< itemKids)
        (\tx -> mkItem2 hash tx itemKids)
        valueMaybe
  where
    mkItem2 hash tx itemKids = case tx of
      LeInitial x -> mkItem3 hash tx :< itemKids
      LeWiring  w -> mkItem3 hash tx :< (fromNets w.wiring.nets) <> (fromDiagrams w.wiring.diagrams) <> itemKids
      LeFiring  f -> mkItem3 hash tx :< itemKids

    mkItem3 hash tx    = mkItem (caption hash tx) (Just $ toRoute hash tx)

    fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ NetR     hash ix n.name) :< []) nets
    fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ DiagramR hash ix d.name) :< []) diags

    caption :: HashStr -> TxSum -> String
    caption hash = case _ of
      LeInitial x -> "🌐 "  <> shortHash hash
      LeWiring  w -> "🥨 " <> shortHash hash
      LeFiring  f -> "🔥 " <> shortHash hash

    toRoute :: HashStr -> TxSum -> Route
    toRoute hash = case _ of
      LeInitial x -> NamespaceR x
      LeWiring  w -> WiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash }
      LeFiring  f -> FiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash }

    -- TODO we need to return a Route currently, but we may want to return a (LoadFromHash ... ::Query) instead,
    -- so we could load unloaded hashes from the menu.
    unloadedRoute = Nothing
