module View.Studio.Model where

import Prelude
import Affjax (URL) -- TODO introduce URL alias in Client so we can abstract Affjax away
import Data.Array (index, findIndex, modifyAt)
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Bifunctor (bimap)
import Data.Either (hush)
import Data.Either.Nested (type (\/))
import Data.Foldable (find)
import Data.Lens (preview)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Record as Record

import Data.Petrinet.Representation.PNPRO as PNPRO
import Data.Petrinet.Representation.PNPROtoDict as PNPRO
import Statebox.Core.Types (Diagram)
import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, WiringTx)
import Statebox.Core.Lenses (_wiringTx, _firingTx)
import View.Diagram.Model (DiagramInfo)
import View.Model (Project, ProjectName, NetInfoWithTypesAndRoles)
import View.Petrinet.Model (NetInfo)
import View.Studio.Model.Route
import View.Studio.Model.TxCache as TxCache
import View.Studio.Model.TxCache (ExecutionTrace)

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

  | CreateProject
  | DeleteProject ProjectName
  | CreateKDMonCat

type State =
  { route       :: Route
  , projects    :: Array Project
  , hashSpace   :: AdjacencySpace HashStr TxSum -- ^ Hashes and their (tree of) links.
  , title       :: String
  , msg         :: String
  , apiUrl      :: URL
  , menuItems   :: Array (String /\ Maybe Route)
  }

--------------------------------------------------------------------------------

resolveRoute :: Route -> State -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
resolveRoute route state = case route of
  Home                              -> pure $ ResolvedHome state.projects
  TxHome                            -> pure $ ResolvedTxHome state.projects
  ProjectRoute projectName pr       -> findProject state.projects projectName >>= resolveProjectRoute pr state
  ApiRoute x                        -> resolveApiRoute x state.hashSpace

resolveProjectRoute :: ProjectRoute DiagramName NetName -> State -> Project -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
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

resolveApiRoute :: ApiRoute -> AdjacencySpace HashStr TxSum -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
resolveApiRoute route hashSpace = case route of
  UberRootR  url                    -> pure $ ResolvedUberRoot url
  NamespaceR hash                   -> pure $ ResolvedNamespace hash
  WiringR    x                      -> ResolvedWiring x <$> TxCache.findWiringTx hashSpace x.hash
  FiringR    x                      -> ResolvedFiring x <$> firingTxM <*> pure execTrace
    where
      firingTxM = TxCache.findFiringTx hashSpace x.hash
      execTrace = TxCache.findExecutionTrace hashSpace x.hash execHash
      execHash  = firingTxM >>= _.firing.execution # fromMaybe x.hash
  DiagramR   wiringHash ix name     -> (\d -> ResolvedDiagram d Nothing) <$> TxCache.findDiagramInfo hashSpace wiringHash ix
  NetR       wiringHash ix name     -> (\n -> ResolvedNet     n)         <$> TxCache.findNetInfo     hashSpace wiringHash ix

--------------------------------------------------------------------------------

findProject :: Array Project -> ProjectName -> Maybe Project
findProject projects projectName = find (\p -> p.name == projectName) projects

modifyProject :: ProjectName -> (Project -> Project) -> Array Project -> Maybe (Array Project)
modifyProject projectName fn projects = do
  ix <- findIndex (\p -> p.name == projectName) projects
  modifyAt ix fn projects

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
modifyKDMonCat diagramId f = spy "kdmoncat" <<< Map.alter (map f) diagramId

--------------------------------------------------------------------------------

fromPNPROProject :: PNPRO.Project -> Project
fromPNPROProject project =
  { projectId: mempty
  , name:      project.name
  , nets:      PNPRO.toNetInfo <$> project.gspn
  , diagrams:  mempty
  , kdmoncats: mempty
  , roleInfos: mempty
  , types:     mempty
  }
