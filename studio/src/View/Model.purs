module View.Model where

import Prelude

import Data.Map (Map)
import Data.Tuple.Nested (type (/\))

import Data.Auth (RoleInfo)
import Data.Typedef (Typedef)
import Data.Typedef.Typedef2 (Typedef2)
import KDMoncat.Input.String (Input) as KDMoncat.Input.String
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRolesFRow)

type Project =
  { name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , kdmoncats    :: Map String KDMoncat.Input.String.Input
  , roleInfos    :: Array RoleInfo
  , types        :: Array (String /\ Typedef2)
  }

type ProjectName = String

--------------------------------------------------------------------------------

type NetInfoWithTypesAndRoles = Record (NetInfoWithTypesAndRolesFRow PID TID Typedef Typedef2 ())
