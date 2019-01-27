module View.Studio.Route where

import Prelude

type NetName = String

type DiagramName = String

data RouteF p
  = Home
  | Net     p NetName
  | Diagram p DiagramName
  | Types   p
  | Auths   p

derive instance functorRouteF :: Functor RouteF
derive instance eqRouteF :: Eq p => Eq (RouteF p)

type Route = RouteF String
