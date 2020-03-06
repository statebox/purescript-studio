module View.Studio.Model.Route where

import Prelude
import Affjax (URL)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))

import KDMoncat.Input.String (Input) as KDMoncat.Input.String
import Statebox.Core.Types (NetsAndDiagramsIndex)
import Statebox.Core.Transaction (HashStr, Tx, TxSum(..), WiringTx, FiringTx, evalTxSum)
import View.Model (ProjectName)
import View.Studio.Model.TxCache (ExecutionTrace)

type Route = RouteF ProjectName DiagramName NetName

-- | This can:
-- |
-- | - identify objects
-- | - be rendered to breadcrumbs
-- | - be rendered into a menu entry
data RouteF p d n
  = Home

  | ProjectR   p
  | Types      p
  | Auths      p

  -- Project-related constructors
  | Net        p n
  | Diagram    p d (Maybe (NodeIdent d n)) -- ^ A diagram with maybe one of its 'child' nodes.
  | KDMonCatR  p String

  -- Statebox API-related constructors
  | ApiThing ApiRoute

derive instance eqRouteF :: (Eq p, Eq d, Eq n) => Eq (RouteF p d n)
derive instance ordRouteF :: (Ord p, Ord d, Ord n) => Ord (RouteF p d n)

-- | Statebox Core/API-related routes
data ApiRoute
  = UberRootR  URL
  | NamespaceR HashStr
  | WiringR    WiringFiringInfo
  | FiringR    WiringFiringInfo
  | DiagramR   HashStr NetsAndDiagramsIndex String
  | NetR       HashStr NetsAndDiagramsIndex String

derive instance eqApiRoute :: Eq ApiRoute
derive instance ordApiRoute :: Ord ApiRoute

type DiagramName = String

type NetName = String

--------------------------------------------------------------------------------

data ResolvedRouteF p d n
  = ResolvedHome      (Array p)

  | ResolvedProject   p
  | ResolvedTypes     p
  | ResolvedAuths     p

  -- Project-related *and* Statebox API-related constructors
  | ResolvedNet       n
  | ResolvedDiagram   d (Maybe (NodeIdent d n))
  | ResolvedKDMonCat  KDMoncat.Input.String.Input

  -- Statebox API transaction constructors
  | ResolvedUberRoot  URL
  | ResolvedNamespace HashStr
  | ResolvedWiring    WiringFiringInfo WiringTx
  | ResolvedFiring    WiringFiringInfo FiringTx (String \/ ExecutionTrace)

--------------------------------------------------------------------------------

fromTxSum :: ∀ p d n. URL -> HashStr -> TxSum -> RouteF p d n
fromTxSum endpointUrl hash tx = tx # ApiThing <<< evalTxSum
  (\x -> UberRootR endpointUrl)
  (\x -> NamespaceR x.root.message)
  (\w -> WiringR { name: hash, endpointUrl, hash })
  (\f -> FiringR { name: hash, endpointUrl, hash })

--------------------------------------------------------------------------------

data NodeIdent d n = DiagramNode d | NetNode n

derive instance eqNodeIdent :: (Eq d, Eq n) => Eq (NodeIdent d n)
derive instance ordNodeIdent :: (Ord d, Ord n) => Ord (NodeIdent d n)

--------------------------------------------------------------------------------

type WiringFiringInfo =
  { name        :: String
  , endpointUrl :: URL
  , hash        :: HashStr
  }

type NamespaceInfo =
  { name :: String
  , hash :: HashStr
  }
