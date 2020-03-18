module View.Studio.Model.Route where

import Prelude hiding ((/))
import Affjax (URL)
import Data.Either.Nested (type (\/))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype
import Data.Lens.Iso.Newtype
import Data.Tuple.Nested (type (/\))
import Data.Generic.Rep
import Routing.Duplex (RouteDuplex', path, root, segment, string, int, optional, param, params)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax

import View.KDMonCat.App (Input) as KDMonCat.App
import Statebox.Core.Types (NetsAndDiagramsIndex)
import Statebox.Core.Transaction (HashStr, Tx, TxSum(..), WiringTx, FiringTx, evalTxSum)
import View.Model (ProjectId)
import View.Studio.Model.TxCache (ExecutionTrace)

type Route = RouteF ProjectId DiagramName NetName
type ProjectRoute = ProjectRouteF DiagramName NetName

-- | This can:
-- |
-- | - identify objects
-- | - be rendered to breadcrumbs
-- | - be rendered into a menu entry
data RouteF p d n
  = Home
  | TxHome (Maybe HashStr)
  | ProjectRoute p (ProjectRouteF d n)
  | ApiRoute ApiRoute URL

derive instance eqRouteF :: (Eq p, Eq d, Eq n) => Eq (RouteF p d n)
derive instance ordRouteF :: (Ord p, Ord d, Ord n) => Ord (RouteF p d n)
derive instance genericRouteF :: Generic (RouteF p d n) _

codex :: RouteDuplex' Route
codex = root $ sum
  { "Home":         noArgs
  , "TxHome" :      "tx" / optional segment
  , "ProjectRoute": "project" / segment / projectCodex
  , "ApiRoute":     "api" / apiCodex / param "endpointUrl"
  }

-- Project-related routes
data ProjectRouteF d n
  = ProjectHome
  | Types
  | Auths

  | Net       n
  | Diagram   d (Maybe (NodeIdent d n)) -- ^ A diagram with maybe one of its 'child' nodes.
  | KDMonCatR String

derive instance eqProjectRouteF :: (Eq d, Eq n) => Eq (ProjectRouteF d n)
derive instance ordProjectRouteF :: (Ord d, Ord n) => Ord (ProjectRouteF d n)
derive instance genericProjectRouteF :: Generic (ProjectRouteF d n) _

projectCodex :: RouteDuplex' ProjectRoute
projectCodex = sum
  { "ProjectHome": noArgs
  , "Types":       "types" / noArgs
  , "Auths":       "auths" / noArgs
  , "Net":         "net" / segment
  , "Diagram":     "diagram" / segment / optional nodeIdentCodex
  , "KDMonCatR":   "kdmoncat" / segment
  }

-- | Statebox Core/API-related routes
data ApiRoute
  = UberRootR
  | NamespaceR HashStr
  | WiringR    HashStr
  | FiringR    HashStr
  | DiagramR   HashStr NetsAndDiagramsIndex String
  | NetR       HashStr NetsAndDiagramsIndex String

derive instance eqApiRoute :: Eq ApiRoute
derive instance ordApiRoute :: Ord ApiRoute
derive instance genericApiRoute :: Generic ApiRoute _

apiCodex ∷ RouteDuplex' ApiRoute
apiCodex = sum
  { "UberRootR":  noArgs
  , "NamespaceR": path "namespace" $ param "hash"
  , "WiringR":    "wiring" / segment
  , "FiringR":    "firing" / segment
  , "DiagramR":   "diagram" / segment / newtype_ (int segment) / segment
  , "NetR":       "net" / segment / newtype_ (int segment) / segment
  }

type DiagramName = String

type NetName = String

--------------------------------------------------------------------------------

data ResolvedRouteF p d n
  = ResolvedHome      (Map ProjectId p)
  | ResolvedTxHome    (Map ProjectId p)

  | ResolvedProject   p
  | ResolvedTypes     p
  | ResolvedAuths     p

  -- Project-related *and* Statebox API-related constructors
  | ResolvedNet       n
  | ResolvedDiagram   d (Maybe (NodeIdent d n))
  | ResolvedKDMonCat  KDMonCat.App.Input

  -- Statebox API transaction constructors
  | ResolvedUberRoot  URL
  | ResolvedNamespace HashStr
  | ResolvedWiring    WiringFiringInfo WiringTx
  | ResolvedFiring    WiringFiringInfo FiringTx (String \/ ExecutionTrace)

--------------------------------------------------------------------------------

fromTxSum :: ∀ p d n. URL -> HashStr -> TxSum -> RouteF p d n
fromTxSum endpointUrl hash tx = tx # (\x -> ApiRoute x endpointUrl) <<< evalTxSum
  (\x -> UberRootR)
  (\x -> NamespaceR x.root.message)
  (\w -> WiringR hash)
  (\f -> FiringR hash)

--------------------------------------------------------------------------------

data NodeIdent d n = DiagramNode d | NetNode n

derive instance eqNodeIdent :: (Eq d, Eq n) => Eq (NodeIdent d n)
derive instance ordNodeIdent :: (Ord d, Ord n) => Ord (NodeIdent d n)
derive instance genericNodeIdent :: Generic (NodeIdent d n) _

nodeIdentCodex ∷ RouteDuplex' (NodeIdent DiagramName NetName)
nodeIdentCodex = root $ sum
  { "DiagramNode": param "diagram"
  , "NetNode":     param "net"
  }

--------------------------------------------------------------------------------

type WiringFiringInfo =
  { endpointUrl :: URL
  , hash        :: HashStr
  }

type NamespaceInfo =
  { name :: String
  , hash :: HashStr
  }

newtype_ :: forall a b. Newtype a b => RouteDuplex' b -> RouteDuplex' a
newtype_ = _Newtype
