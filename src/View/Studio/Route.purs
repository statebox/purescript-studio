module View.Studio.Route where

import Prelude
import Data.Maybe (Maybe)

type NetName = String

type DiagramName = String

data RouteF p
  = Home
  | Net     p NetName
  | Diagram p DiagramName (Maybe (NodeIdent NetName DiagramName))
  | Types   p
  | Auths   p

derive instance functorRouteF :: Functor RouteF
derive instance eqRouteF :: Eq p => Eq (RouteF p)

type Route = RouteF String

--------------------------------------------------------------------------------

data NodeIdent d n = LeNet n | LeDiagram d

derive instance eqNodeIdent :: (Eq n, Eq d) => Eq (NodeIdent n d)
