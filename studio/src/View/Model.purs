module View.Model where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))

import Data.Auth (RoleInfo)
import Data.Typedef.Typedef2 (Typedef2)
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (NetInfo, NetInfoWithTypesAndRoles)

type Project =
  { name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , roleInfos    :: Array RoleInfo
  , types        :: Array (String /\ Typedef2)
  }

type ProjectName = String