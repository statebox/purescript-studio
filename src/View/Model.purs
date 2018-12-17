module View.Model where

import Prelude
import Data.Auth (RoleInfo)
import Data.Tuple.Nested (type (/\), (/\))
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (NetInfo)
import View.Typedefs.TypedefsEditor (Typedef2(..))

type Project =
  { name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , allRoleInfos :: Array RoleInfo
  , types        :: Array (String /\ Typedef2)
  }

type Diagram = Unit
