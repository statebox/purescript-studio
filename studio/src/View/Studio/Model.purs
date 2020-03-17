module View.Studio.Model where

import Prelude
import Affjax (URL) -- TODO introduce URL alias in Client so we can abstract Affjax away
import Data.Array (findIndex, modifyAt)
import Data.AdjacencySpace (AdjacencySpace)
import Data.Foldable (find)
import Data.Lens (Lens')
import Data.Lens.Record
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol
import Data.Tuple.Nested (type (/\))
import Record as Record
import Routing.PushState (PushStateInterface)
import Web.Event.Event (Event)

import Data.Petrinet.Representation.PNPRO as PNPRO
import Data.Petrinet.Representation.PNPROtoDict as PNPRO
import Statebox.Core.Transaction (HashStr, TxSum)
import View.Diagram.Model (DiagramInfo)
import View.Model (Project, ProjectId, NetInfoWithTypesAndRoles)
import View.Petrinet.Model (NetInfo)
import View.Studio.Model.Route
import View.Studio.Model.TxCache as TxCache

-- deps needed for Action, for now
import View.Petrinet.Model as PetrinetEditor
import View.Diagram.Update as DiagramEditor
import View.KDMonCat.App as KDMonCat.App
import View.KDMonCat.Bricks as KDMonCat.Bricks

--------------------------------------------------------------------------------

data Action
  = SelectRoute Route
  | LoadPNPRO URL
  | SetApiUrl URL
  | LoadTransaction HashStr
  | LoadTransactions URL HashStr
  | ShowDiagramNodeContent Route
  | HandlePetrinetEditorMsg PetrinetEditor.Msg
  | HandleDiagramEditorMsg DiagramEditor.Msg
  | HandleKDMonCatBricksMsg DiagramInfo KDMonCat.Bricks.Output
  | HandleKDMonCatAppMsg KDMonCat.App.Output

  | CRUDProject (CRUDAction Project)
  | CRUDKDMonCat (CRUDAction KDMonCat.App.Input)

  | StopEvent (Maybe Action) Event

data CRUDAction a
  = CreateAction a
  | UpdateAction String (a -> a)
  | DeleteAction String

type State =
  { route       :: Route
  , projects    :: Map ProjectId Project
  , hashSpace   :: AdjacencySpace HashStr TxSum -- ^ Hashes and their (tree of) links.
  , title       :: String
  , msg         :: String
  , apiUrl      :: URL
  , menuItems   :: Array (String /\ Maybe Route)
  , nav         :: PushStateInterface
  }

_projects :: Lens' State (Map ProjectId Project)
_projects = prop (SProxy :: SProxy "projects")

--------------------------------------------------------------------------------

type ResolvedRoute = ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles

resolveRoute :: Route -> State -> Maybe ResolvedRoute
resolveRoute route state = case route of
  Home                       -> pure $ ResolvedHome state.projects
  TxHome       _             -> pure $ ResolvedTxHome state.projects
  ProjectRoute projectId pr  -> findProject state.projects projectId >>= resolveProjectRoute pr state
  ApiRoute     x endpointUrl -> resolveApiRoute endpointUrl x state.hashSpace

resolveProjectRoute :: ProjectRoute -> State -> Project -> Maybe ResolvedRoute
resolveProjectRoute route state project = case route of
  ProjectHome           -> pure $ ResolvedProject project
  Types                 -> pure $ ResolvedTypes project
  Auths                 -> pure $ ResolvedAuths project
  Net       name        -> ResolvedNet <$> findNetInfoWithTypesAndRoles project name
  Diagram   name nodeId -> do diagram <- findDiagramInfo project name
                              let node = nodeId >>= case _ of
                                           DiagramNode dn -> DiagramNode <$> findDiagramInfo              project dn
                                           NetNode     nn -> NetNode     <$> findNetInfoWithTypesAndRoles project nn
                              pure $ ResolvedDiagram diagram node
  KDMonCatR str         -> ResolvedKDMonCat <$> findKDMonCat project str

resolveApiRoute :: URL -> ApiRoute -> AdjacencySpace HashStr TxSum -> Maybe ResolvedRoute
resolveApiRoute endpointUrl route hashSpace = case route of
  UberRootR                         -> pure $ ResolvedUberRoot endpointUrl
  NamespaceR hash                   -> pure $ ResolvedNamespace hash
  WiringR    hash                   -> ResolvedWiring { hash, endpointUrl } <$> TxCache.findWiringTx hashSpace hash
  FiringR    hash                   -> ResolvedFiring { hash, endpointUrl } <$> firingTxM <*> pure execTrace
    where
      firingTxM = TxCache.findFiringTx hashSpace hash
      execTrace = TxCache.findExecutionTrace hashSpace hash execHash
      execHash  = firingTxM >>= _.firing.execution # fromMaybe hash
  DiagramR   wiringHash ix name     -> (\d -> ResolvedDiagram d Nothing) <$> TxCache.findDiagramInfo hashSpace wiringHash ix
  NetR       wiringHash ix name     -> (\n -> ResolvedNet     n)         <$> TxCache.findNetInfo     hashSpace wiringHash ix

--------------------------------------------------------------------------------

findProject :: Map ProjectId Project -> String -> Maybe Project
findProject projects projectId = Map.lookup projectId projects

findNetInfo :: Project -> NetName -> Maybe NetInfo
findNetInfo project netName = find (\n -> n.name == netName) project.nets

findNetInfoWithTypesAndRoles :: Project -> NetName -> Maybe NetInfoWithTypesAndRoles
findNetInfoWithTypesAndRoles project netName =
  Record.merge { types: project.types, roleInfos: project.roleInfos } <$> findNetInfo project netName

findDiagramInfo :: Project -> DiagramName -> Maybe DiagramInfo
findDiagramInfo project diagramName = find (\d -> d.name == diagramName) project.diagrams

modifyDiagramInfo :: DiagramName -> (DiagramInfo -> DiagramInfo) -> Array DiagramInfo -> Maybe (Array DiagramInfo)
modifyDiagramInfo diagramName fn diagrams = do
  ix <- findIndex (\d -> d.name == diagramName) diagrams
  modifyAt ix fn diagrams

--------------------------------------------------------------------------------

findKDMonCat :: Project -> String -> Maybe KDMonCat.App.Input
findKDMonCat project diagramId = Map.lookup diagramId project.kdmoncats

modifyKDMonCat :: String -> (KDMonCat.App.Input -> KDMonCat.App.Input) -> Map String KDMonCat.App.Input -> Map String KDMonCat.App.Input
modifyKDMonCat diagramId f = Map.alter (map f) diagramId

--------------------------------------------------------------------------------

fromPNPROProject :: PNPRO.Project -> Project
fromPNPROProject project =
  { name:      project.name
  , nets:      PNPRO.toNetInfo <$> project.gspn
  , diagrams:  mempty
  , kdmoncats: mempty
  , roleInfos: mempty
  , types:     mempty
  }
