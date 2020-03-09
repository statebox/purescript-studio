module View.Model where

import Prelude

import Data.Map (Map)
import Data.Tuple.Nested (type (/\))

import Data.Auth (RoleInfo)
import Data.Typedef (Typedef)
import Data.Typedef.Typedef2 (Typedef2)
import View.KDMonCat.App (Input) as KDMonCat.App
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRolesFRow)

type Project =
  { name         :: ProjectName
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , kdmoncats    :: Map String KDMonCat.App.Input
  , roleInfos    :: Array RoleInfo
  , types        :: Array (String /\ Typedef2)
  }

type ProjectName = String

emptyProject :: Project
emptyProject = mempty

--------------------------------------------------------------------------------

type NetInfoWithTypesAndRoles = Record (NetInfoWithTypesAndRolesFRow PID TID Typedef Typedef2 ())
