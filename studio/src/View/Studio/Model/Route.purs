module View.Studio.Model.Route where

import Prelude
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show
import Effect.Aff (Aff)
import Halogen (HalogenIO)
import Affjax (URL)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)
import Routing.Duplex (parse, print) as Duplex
import View.Model (ProjectName)
import Statebox.Core.Execution (PathElem)
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
  | UberRootR  URL
  | NamespaceR HashStr
  | WiringR    WiringFiringInfo
  | FiringR    WiringFiringInfo
  | DiagramR   HashStr PathElem String
  | NetR       HashStr PathElem String

derive instance eqRouteF :: (Eq p, Eq d, Eq n) => Eq (RouteF p d n)
derive instance ordRouteF :: (Ord p, Ord d, Ord n) => Ord (RouteF p d n)

derive instance genericRouteF :: GR.Generic (RouteF p d n) _

instance showRouteF :: (Show p, Show d, Show n) => Show (RouteF p d n) where
  show = genericShow

type DiagramName = String

type NetName = String

data ResolvedRouteF p d n
  = ResolvedHome
  | ResolvedTypes     p
  | ResolvedAuths     p

  -- Project-related *and* Statebox API-related constructors
  | ResolvedNet       n
  | ResolvedDiagram   d (Maybe (NodeIdent d n))

  -- Statebox API transaction constructors
  | ResolvedUberRoot  URL
  | ResolvedNamespace HashStr
  | ResolvedWiring    WiringFiringInfo WiringTx
  | ResolvedFiring    WiringFiringInfo FiringTx (String \/ ExecutionTrace)

--------------------------------------------------------------------------------

data URLRoute = HomePage | Tx String

derive instance genericURLRoute :: GR.Generic URLRoute _
derive instance eqRoute :: Eq URLRoute

urlRoute ∷ RouteDuplex' URLRoute
urlRoute = root $ sum
  { "HomePage" : noArgs
  , "Tx": "tx" / segment
  }

parse ∷ String -> Either RouteError URLRoute
parse = Duplex.parse urlRoute

print ∷ URLRoute -> String
print = Duplex.print urlRoute

type IO = HalogenIO Query Void Aff
type Input = Unit
data Query a = Navigate URLRoute a

--------------------------------------------------------------------------------

data NodeIdent d n = DiagramNode d | NetNode n

derive instance eqNodeIdent :: (Eq d, Eq n) => Eq (NodeIdent d n)
derive instance ordNodeIdent :: (Ord d, Ord n) => Ord (NodeIdent d n)

derive instance genericNodeIdent :: GR.Generic (NodeIdent d n) _

instance showNodeIdent :: (Show d, Show n) => Show (NodeIdent d n) where
  show = genericShow

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

-- TODO we may want to change the name and the exact type a bit; this is a 1st version to get things going
type ExecutionTrace = Array (HashStr /\ Maybe TxSum)
