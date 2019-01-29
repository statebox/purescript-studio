module View.Studio.Route where

import Prelude
import Data.Maybe (Maybe)
import View.Model (ProjectName)

type Route = RouteF ProjectName DiagramName NetName

data RouteF p d n
  = Home
  | Net     p n
  | Diagram p d (Maybe (NodeIdent d n)) -- ^ A diagram with maybe one of its 'child' nodes.
  | Types   p
  | Auths   p

derive instance eqRouteF :: (Eq p, Eq d, Eq n) => Eq (RouteF p d n)
derive instance ordRouteF :: (Ord p, Ord d, Ord n) => Ord (RouteF p d n)

type DiagramName = String

type NetName = String

--------------------------------------------------------------------------------

data NodeIdent d n = LeDiagram d | LeNet n

derive instance eqNodeIdent :: (Eq d, Eq n) => Eq (NodeIdent d n)
derive instance ordNodeIdent :: (Ord d, Ord n) => Ord (NodeIdent d n)
