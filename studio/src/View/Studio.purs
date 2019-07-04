module View.Studio where

import Prelude hiding (div)
import Affjax as Affjax
import Affjax (URL)
import Affjax.ResponseFormat as ResponseFormat
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Coroutine (Consumer, Producer, Process, runProcess, consumer, connect)
import Data.Array (cons, index)
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Either (either, hush)
import Data.Either.Nested (type (\/), Either3)
import Data.Foldable (find, foldMap)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (preview)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (guard)
import Data.Set as Set
import Data.String.CodePoints (take)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Effect.Exception (try)
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
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes, src, href, placeholder)
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Query.HalogenM (HalogenM(..))
import Record as Record
import TreeMenu as ObjectTree
import TreeMenu (mkItem, MenuTree)

import Data.Diagram.FromNLL as FromNLL
import Data.Diagram.FromNLL (ErrDiagramEncoding)
import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.PNPRO as PNPRO
import Data.Petrinet.Representation.PNPROtoDict as PNPRO
import Statebox.Client as Stbx
import Statebox.Client (evalTransactionResponse)
import Statebox.Core.Execution (PathElem)
import Statebox.Core.Types (Diagram)
import Statebox.Core.Transaction as Stbx
import Statebox.Core.Transaction (HashStr, HashTx, TxSum, WiringTx, FiringTx, evalTxSum)
import Statebox.Core.Lenses (_leWiring, _leFiring)
import Statebox.Core.Transaction.Codec (DecodingError(..))
import View.Auth.RolesEditor as RolesEditor
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Model (Project, ProjectName)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRoles, Msg(NetUpdated))
import View.Petrinet.Model.NLL as NLL
import View.Studio.Route (Route, RouteF(..), ResolvedRouteF(..), NetName, DiagramName, NodeIdent(..))
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

type State =
  { route       :: Route
  , projects    :: Array Project
  , hashSpace   :: AdjacencySpace HashStr TxSum -- ^ Hashes and their (tree of) links.
  , msg         :: String
  }

--------------------------------------------------------------------------------

data Query a
  = SelectRoute Route a
  | LoadPNPRO URL a
  | LoadTransaction URL HashStr a
  | LoadTransactions URL HashStr a
  | HandleObjectTreeMsg (ObjectTree.Msg Route) a
  | HandlePetrinetEditorMsg Msg a
  | HandleDiagramEditorMsg DiagramEditor.Msg a

type ChildQuery = Coproduct3 (ObjectTree.Query Route) (PetrinetEditor.QueryF PID TID) DiagramEditor.Query

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

      LoadTransaction endpointUrl hash next -> do
        H.liftEffect $ log $ "LoadTransaction: requesting transaction " <> hash <> " from " <> endpointUrl
        res <- H.liftAff $ Stbx.requestTransaction endpointUrl hash
        res # evalTransactionResponse
          (\err                 -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (\(DecodingError err) -> H.liftEffect $ log $ "Expected to decode a valid Statebox transaction: " <> show err)
          (\{id, tx}            -> do H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
                                      H.liftEffect $ log $ show tx)
        pure next

      LoadTransactions endpointUrl startHash next -> do
        H.liftEffect $ log $ "LoadTransactions: requesting transactions up to root, starting at " <> startHash <> " from " <> endpointUrl
        let
          txProducer :: Producer HashTx (HalogenM State Query _ _ Void m) Unit
          txProducer = Stbx.requestTransactionsToRootM endpointUrl startHash

          txConsumer :: Consumer HashTx (HalogenM State Query _ _ Void m) Unit
          txConsumer = consumer txStorer
            where
              txStorer :: HashTx -> HalogenM State Query _ _ Void m (Maybe _)
              txStorer itx@{id, tx} = do
                H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
                H.liftEffect $ log $ show itx
                pure Nothing

          -- | This ingests transactions from the HTTP API into our transaction storage.
          txIngester :: Process (HalogenM State Query _ _ Void m) Unit
          txIngester = txProducer `connect` txConsumer

        runProcess txIngester
        pure next

      LoadPNPRO url next -> do
        H.liftEffect $ log $ "LoadPNPRO: requesting PNPRO file from " <> url
        res <- H.liftAff $ Affjax.request $ Affjax.defaultRequest { url = url, responseFormat = ResponseFormat.string }
        res.body # either
          (\err -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (\body -> do
               pnproDocumentE <- H.liftEffect $ try $ PNPRO.fromString body
               pnproDocumentE # either
                 (\err      -> H.liftEffect $ log $ "Error decoding PNPRO document: " <> show err)
                 (\pnproDoc -> H.modify_ $ \state -> state { projects = fromPNPROProject pnproDoc.project `cons` state.projects })
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
                           , placeholder "Enter Statebox Cloud transaction hash"
                           , HE.onValueInput $ HE.input (LoadTransactions Ex.endpointUrl)
                           , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
                           ]
                , br []
                , br []
                , HH.input [ HP.value ""
                           , placeholder "Enter http URL to PNPRO file"
                           , HE.onValueInput $ HE.input LoadPNPRO
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

          ResolvedUberRoot url ->
            text $ "Service über-root " <> url

          ResolvedNamespace hash ->
            text $ "Namespace " <> hash

          ResolvedWiring wfi wiringTx ->
            div []
                [ text $ "Wiring " <> wfi.hash <> " at " <> wfi.endpointUrl <> "."
                , br [], br []
                , pre [] [ text $ show wiringTx ]
                ]

          ResolvedFiring wfi firingTx ->
            div []
                [ text $ "Firing " <> wfi.hash <> " at " <> wfi.endpointUrl <> "."
                , br []
                , p [] [ pre [] [ text $ show firingTx ] ]
                , br []
                , p [] [ text $ "message: " <> show firingTx.firing.message ]
                , br []
                , p [] [ text $ "path: " <> show firingTx.firing.path ]
                ]

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
                               UberRootR  url                -> [ "über-namespace", url ]
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
resolveRoute route {projects, hashSpace} = case route of
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
  UberRootR  url                    -> pure $ ResolvedUberRoot url
  NamespaceR hash                   -> pure $ ResolvedNamespace hash
  WiringR    x                      -> ResolvedWiring x <$> findWiringTx hashSpace x.hash
  FiringR    x                      -> ResolvedFiring x <$> findFiringTx hashSpace x.hash
  DiagramR   wiringHash ix name     -> (\d -> ResolvedDiagram d Nothing) <$> findDiagramInfoInWirings hashSpace wiringHash ix
  NetR       wiringHash ix name     -> (\n -> ResolvedNet     n)         <$> findNetInfoInWirings     hashSpace wiringHash ix

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects

findNetInfo :: Project -> NetName -> Maybe NetInfo
findNetInfo project netName = find (\n -> n.name == netName) project.nets

findNetInfoWithTypesAndRoles :: Project -> NetName -> Maybe NetInfoWithTypesAndRoles
findNetInfoWithTypesAndRoles project netName =
  Record.merge { types: project.types, roleInfos: project.roleInfos } <$> findNetInfo project netName

findDiagramInfo :: Project -> DiagramName -> Maybe DiagramInfo
findDiagramInfo project diagramName = find (\d -> d.name == diagramName) project.diagrams

findWiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe WiringTx
findWiringTx hashSpace wiringHash = preview _leWiring =<< AdjacencySpace.lookup wiringHash hashSpace

findFiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe FiringTx
findFiringTx hashSpace firingHash = preview _leFiring =<< AdjacencySpace.lookup firingHash hashSpace

findNetInfoInWirings :: AdjacencySpace HashStr TxSum -> HashStr -> PathElem -> Maybe NetInfoWithTypesAndRoles
findNetInfoInWirings hashSpace wiringHash ix = do
  wiring      <- findWiringTx hashSpace wiringHash
  netW        <- spy "findNetInfoInWirings: netW"    $ wiring.wiring.nets `index` ix
  netTopo     <- spy "findNetInfoInWirings: netTopo" $ Net.fromNLLMaybe 0 netW.partition
  let
    placeNames = NLL.defaultPlaceNames netTopo
    netInfo    = spy "findNetInfoInWirings: netInfo" $ NLL.toNetInfoWithDefaults netTopo netW.name placeNames netW.names
  pure $ Record.merge { types: [], roleInfos: [] } netInfo

findDiagramInfoInWirings :: AdjacencySpace HashStr TxSum -> HashStr -> PathElem -> Maybe DiagramInfo
findDiagramInfoInWirings hashSpace wiringHash ix =
  hush =<< diagramEitherMaybe
  where
    diagramEitherMaybe :: Maybe (ErrDiagramEncoding \/ DiagramInfo)
    diagramEitherMaybe = (\d -> FromNLL.fromNLL d.name (toNLL d)) <$> diagramMaybe

    diagramMaybe :: Maybe Diagram
    diagramMaybe = (flip index ix <<< _.wiring.diagrams) =<< findWiringTx hashSpace wiringHash

    toNLL d = [d.width] <> d.pixels

--------------------------------------------------------------------------------

stateMenu :: State -> MenuTree Route
stateMenu { projects, hashSpace } =
  mkItem "Studio" Nothing :< (txItems <> projectItems)
  where
    txItems        = AdjacencySpace.unsafeToTree transactionMenu hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)
    projectItems   = projectMenu <$> projects

projectMenu :: Project -> MenuTree Route
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

transactionMenu :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe TxSum -> Array (MenuTree Route) -> MenuTree Route
transactionMenu t hash valueMaybe itemKids =
  maybe (mkUnloadedItem itemKids)
        (\tx -> mkItem2 hash tx itemKids)
        valueMaybe
  where
    mkItem2 :: HashStr -> TxSum -> Array (MenuTree Route) -> MenuTree Route
    mkItem2 hash tx itemKids = evalTxSum
      (\x -> mkItem ("☁️ "  <> shortHash hash)
                    (Just $ UberRootR Ex.endpointUrl)
                    :< itemKids
      )
      (\x -> mkItem ("🌐 "  <> shortHash hash)
                    (Just $ NamespaceR x.root.message)
                    :< itemKids
      )
      (\w -> mkItem ("🥨 " <> shortHash hash)
                    (Just $ WiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash })
                    :< (fromNets w.wiring.nets <> fromDiagrams w.wiring.diagrams <> itemKids)
      )
      (\f -> mkItem ("🔥 " <> shortHash hash)
                    (Just $ FiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash })
                    :< itemKids
      ) tx
      where
        fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ NetR     hash ix n.name) :< []) nets
        fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ DiagramR hash ix d.name) :< []) diags

    mkUnloadedItem :: Array (MenuTree Route) -> MenuTree Route
    mkUnloadedItem itemKids = mkItem ("👻 " <> shortHash hash) unloadedRoute :< itemKids
      where
        -- TODO we need to return a Route currently, but we may want to return a (LoadTransaction ... ::Query) instead,
        -- so we could load unloaded hashes from the menu.
        unloadedRoute = Nothing

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8

--------------------------------------------------------------------------------

fromPNPROProject :: PNPRO.Project -> Project
fromPNPROProject project =
  { name:      project.name
  , nets:      PNPRO.toNetInfo <$> project.gspn
  , diagrams:  mempty
  , roleInfos: mempty
  , types:     mempty
  }
