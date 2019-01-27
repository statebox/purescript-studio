module View.Studio.Route where

import Prelude

import View.Diagram.Model (DiagramInfo)

type NetName = String

type DiagramName = String

data RouteF p
  = Home
  | Net     p NetName
  | Diagram p DiagramName
  | Types   p
  | Auths   p

derive instance functorRouteF :: Functor RouteF

type Route = RouteF String

-- TODO this has a default case, which is dangerous
routesObjNameEq :: forall p. Eq p => RouteF p -> RouteF p -> Boolean
routesObjNameEq r1 r2 = case r1, r2 of
  Net     pn n, Net     pn' n' -> pn == pn' && n == n'
  Diagram pn d, Diagram pn' d' -> pn == pn' && d == d'
  Types   pn  , Types   pn'    -> pn == pn'
  Auths   pn  , Auths   pn'    -> pn == pn'
  Home        , Home           -> true
  _           , _              -> false
