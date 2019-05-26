module View.Studio.Route where

import Prelude
import Affjax (URL)
import Data.Maybe (Maybe)
import View.Model (ProjectName)
import Statebox.Core.Types (PathElem)
import Statebox.Core.Transaction (HashStr, Tx, TxSum(..), WiringTx, FiringTx)

type Route = RouteF ProjectName DiagramName NetName

-- | This can:
-- |
-- | - identify objects
-- | - be rendered to breadcrumbs
-- | - be rendered into a menu entry
data RouteF p d n
  = Home
  | Types      p
  | Auths      p

  -- Project-related constructors
  | Net        p n
  | Diagram    p d (Maybe (NodeIdent d n)) -- ^ A diagram with maybe one of its 'child' nodes.

  -- Statebox API-related constructors
  | NamespaceR HashStr
  | WiringR    WiringFiringInfo
  | FiringR    WiringFiringInfo
  | DiagramR   HashStr PathElem String
  | NetR       HashStr PathElem String

derive instance eqRouteF :: (Eq p, Eq d, Eq n) => Eq (RouteF p d n)
derive instance ordRouteF :: (Ord p, Ord d, Ord n) => Ord (RouteF p d n)

type DiagramName = String

type NetName = String

data ResolvedRouteF p d n
  = ResolvedHome
  | ResolvedNet       n
  | ResolvedDiagram   d (Maybe (NodeIdent d n))
  | ResolvedTypes     p
  | ResolvedAuths     p

  -- Statebox API-specific constructors
  | ResolvedNamespace HashStr
  | ResolvedWiring    WiringFiringInfo WiringTx
  | ResolvedFiring    WiringFiringInfo FiringTx

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
