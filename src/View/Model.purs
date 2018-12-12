module View.Model where

import Prelude
import Data.Auth (RoleInfo)
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (NetInfo)

type Project =
  { name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , allRoleInfos :: Array RoleInfo
  }

type Diagram = Unit
