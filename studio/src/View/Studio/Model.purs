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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Record as Record

import Data.Diagram.FromNLL as FromNLL
import Data.Diagram.FromNLL (ErrDiagramEncoding)
import Data.Petrinet.Representation.NLL as Net
import Data.Petrinet.Representation.PNPRO as PNPRO
import Data.Petrinet.Representation.PNPROtoDict as PNPRO
import Statebox.Core.Types (Diagram, NetsAndDiagramsIndex(..))
import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, WiringTx)
import Statebox.Core.Lenses (_wiringTx, _firingTx)
import View.Diagram.Model (DiagramInfo)
import View.Model (Project, ProjectName, NetInfoWithTypesAndRoles)
import View.Petrinet.Model (NetInfo)
import View.Petrinet.Model.NLL as NLL
import View.Studio.Model.Route (ApiRoute(..), Route, RouteF(..), ResolvedRouteF(..), NetName, DiagramName, NodeIdent(..), ExecutionTrace)

-- deps needed for Action, for now
import View.Petrinet.Model as PetrinetEditor
import View.Diagram.Update as DiagramEditor
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
  | HandleKDMonCatMsg DiagramInfo KDMonCat.Bricks.Output

type State =
  { route       :: Route
  , projects    :: Array Project
  , hashSpace   :: AdjacencySpace HashStr TxSum -- ^ Hashes and their (tree of) links.
  , title       :: String
  , msg         :: String
  , apiUrl      :: URL
  }

--------------------------------------------------------------------------------

resolveRoute :: RouteF ProjectName DiagramName NetName -> State -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
resolveRoute route state = case route of
  Home                              -> pure ResolvedHome
  Types     projectName             -> ResolvedTypes <$> findProject state.projects projectName
  Auths     projectName             -> ResolvedAuths <$> findProject state.projects projectName
  Net       projectName name        -> do project <- findProject state.projects projectName
                                          net     <- findNetInfoWithTypesAndRoles project name
                                          pure $ ResolvedNet net
  Diagram   projectName name nodeId -> do project <- findProject state.projects projectName
                                          diagram <- findDiagramInfo project name
                                          let node = nodeId >>= case _ of
                                                       DiagramNode dn -> DiagramNode <$> findDiagramInfo              project dn
                                                       NetNode     nn -> NetNode     <$> findNetInfoWithTypesAndRoles project nn
                                          pure $ ResolvedDiagram diagram node
  ApiThing x -> resolveApiRoute x state.hashSpace

resolveApiRoute :: ApiRoute -> AdjacencySpace HashStr TxSum -> Maybe (ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles)
resolveApiRoute route hashSpace = case route of
  UberRootR  url                    -> pure $ ResolvedUberRoot url
  NamespaceR hash                   -> pure $ ResolvedNamespace hash
  WiringR    x                      -> ResolvedWiring x <$> findWiringTx hashSpace x.hash
  FiringR    x                      -> ResolvedFiring x <$> firingTxM <*> pure execTrace
    where
      firingTxM = findFiringTx hashSpace x.hash
      execTrace = findExecutionTrace hashSpace x.hash execHash
      execHash  = firingTxM >>= _.firing.execution # fromMaybe x.hash
  DiagramR   wiringHash ix name     -> (\d -> ResolvedDiagram d Nothing) <$> findDiagramInfoInWirings hashSpace wiringHash ix
  NetR       wiringHash ix name     -> (\n -> ResolvedNet     n)         <$> findNetInfoInWirings     hashSpace wiringHash ix

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

findWiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe WiringTx
findWiringTx hashSpace wiringHash = preview _wiringTx =<< AdjacencySpace.lookup wiringHash hashSpace

findFiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe FiringTx
findFiringTx hashSpace firingHash = preview _firingTx =<< AdjacencySpace.lookup firingHash hashSpace

findNetInfoInWirings :: AdjacencySpace HashStr TxSum -> HashStr -> NetsAndDiagramsIndex -> Maybe NetInfoWithTypesAndRoles
findNetInfoInWirings hashSpace wiringHash ix = do
  wiring      <- findWiringTx hashSpace wiringHash
  netW        <- spy "findNetInfoInWirings: netW"    $ wiring.wiring.nets `index` (un NetsAndDiagramsIndex ix)
  netTopo     <- spy "findNetInfoInWirings: netTopo" $ Net.fromNLLMaybe 0 netW.partition
  let
    placeNames = NLL.defaultPlaceNames netTopo
    netInfo    = spy "findNetInfoInWirings: netInfo" $ NLL.toNetInfoWithDefaults netTopo netW.name placeNames netW.names
  pure $ Record.merge { types: [], roleInfos: [] } netInfo

findDiagramInfoInWirings :: AdjacencySpace HashStr TxSum -> HashStr -> NetsAndDiagramsIndex -> Maybe DiagramInfo
findDiagramInfoInWirings hashSpace wiringHash ix =
  hush =<< diagramEitherMaybe
  where
    diagramEitherMaybe :: Maybe (ErrDiagramEncoding \/ DiagramInfo)
    diagramEitherMaybe = (\d -> FromNLL.fromNLL d.name (toNLL d)) <$> diagramMaybe

    diagramMaybe :: Maybe Diagram
    diagramMaybe = (flip index (un NetsAndDiagramsIndex ix) <<< _.wiring.diagrams) =<< findWiringTx hashSpace wiringHash

    toNLL d = [d.width] <> d.pixels

findExecutionTrace :: AdjacencySpace HashStr TxSum -> HashStr -> HashStr -> String \/ ExecutionTrace
findExecutionTrace s firingHash executionHash =
  hashChainE # bimap (const "Failed to resolve execution trace, probably because a parent hash was missing from the space.")
                     (map (\hash -> hash /\ AdjacencySpace.lookup hash s))
  where
    hashChainE :: Array HashStr \/ Array HashStr
    hashChainE = AdjacencySpace.unsafeAncestorsBetween s firingHash executionHash

--------------------------------------------------------------------------------

fromPNPROProject :: PNPRO.Project -> Project
fromPNPROProject project =
  { name:      project.name
  , nets:      PNPRO.toNetInfo <$> project.gspn
  , diagrams:  mempty
  , roleInfos: mempty
  , types:     mempty
  }
