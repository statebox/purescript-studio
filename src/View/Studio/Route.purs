module View.Studio.Route where

import Prelude

import View.Petrinet.Model (NetInfo)
import View.Diagram.Model (DiagramInfo)

data RouteF p
  = Home
  | Net     p NetInfo
  | Diagram p DiagramInfo
  | Types   p

derive instance functorRouteF :: Functor RouteF

type Route = RouteF String

routesObjNameEq :: forall p. Eq p => RouteF p -> RouteF p -> Boolean
routesObjNameEq r1 r2 = case r1, r2 of
  Net     pn n, Net     pn' n' -> pn == pn' && n.name == n'.name
  Diagram pn d, Diagram pn' d' -> pn == pn' && d.name == d'.name
  Types   pn  , Types   pn'    -> pn == pn'
  _           , _              -> false

