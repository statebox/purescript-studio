module View.Studio.Model.TxCache where

import Prelude
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
import Statebox.Core.Types (Diagram, NetsAndDiagramsIndex(..))
import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, WiringTx)
import Statebox.Core.Lenses (_wiringTx, _firingTx)
import View.Diagram.Model (DiagramInfo)
import View.Model (NetInfoWithTypesAndRoles)
import View.Petrinet.Model (NetInfo)
import View.Petrinet.Model.NLL as NLL

findWiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe WiringTx
findWiringTx hashSpace wiringHash = preview _wiringTx =<< AdjacencySpace.lookup wiringHash hashSpace

findFiringTx :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe FiringTx
findFiringTx hashSpace firingHash = preview _firingTx =<< AdjacencySpace.lookup firingHash hashSpace

findNetInfo :: AdjacencySpace HashStr TxSum -> HashStr -> NetsAndDiagramsIndex -> Maybe NetInfoWithTypesAndRoles
findNetInfo hashSpace wiringHash ix = do
  wiring      <- findWiringTx hashSpace wiringHash
  netW        <- spy "TxCache.findNetInfo: netW"    $ wiring.wiring.nets `index` (un NetsAndDiagramsIndex ix)
  netTopo     <- spy "TxCache.findNetInfo: netTopo" $ Net.fromNLLMaybe 0 netW.partition
  let
    placeNames = NLL.defaultPlaceNames netTopo
    netInfo    = spy "TxCache.findNetInfo: netInfo" $ NLL.toNetInfoWithDefaults netTopo netW.name placeNames netW.names
  pure $ Record.merge { types: [], roleInfos: [] } netInfo

findDiagramInfo :: AdjacencySpace HashStr TxSum -> HashStr -> NetsAndDiagramsIndex -> Maybe DiagramInfo
findDiagramInfo hashSpace wiringHash ix =
  hush =<< diagramEitherMaybe
  where
    diagramEitherMaybe :: Maybe (ErrDiagramEncoding \/ DiagramInfo)
    diagramEitherMaybe = (\d -> FromNLL.fromNLL d.name (toNLL d)) <$> diagramMaybe

    diagramMaybe :: Maybe Diagram
    diagramMaybe = (flip index (un NetsAndDiagramsIndex ix) <<< _.wiring.diagrams) =<< findWiringTx hashSpace wiringHash

    toNLL d = [d.width] <> d.pixels

--------------------------------------------------------------------------------

-- TODO we may want to change the name and the exact type a bit; this is a 1st version to get things going
type ExecutionTrace = Array (HashStr /\ Maybe TxSum)

findExecutionTrace :: AdjacencySpace HashStr TxSum -> HashStr -> HashStr -> String \/ ExecutionTrace
findExecutionTrace s firingHash executionHash =
  hashChainE # bimap (const "Failed to resolve execution trace, probably because a parent hash was missing from the space.")
                     (map (\hash -> hash /\ AdjacencySpace.lookup hash s))
  where
    hashChainE :: Array HashStr \/ Array HashStr
    hashChainE = AdjacencySpace.unsafeAncestorsBetween s firingHash executionHash
