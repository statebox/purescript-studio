module View.Model where

import Prelude

import Data.Map (Map, fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.FoldableWithIndex

import Data.Auth (RoleInfo)
import Data.Typedef (Typedef)
import Data.Typedef.Typedef2 (Typedef2)
import View.KDMonCat.App (Input) as KDMonCat.App
import View.Diagram.Model (DiagramInfo)
import View.Petrinet.Model (PID, TID, NetInfo, NetInfoWithTypesAndRolesFRow)

type Project =
  { projectId    :: String
  , name         :: ProjectName
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

--------------------------------------------------------------------------------

type User =
  { email :: String
  , uid :: String
  }

type ProjectJS =
  { projectId    :: String
  , name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , kdmoncats    :: Array { name :: String, input :: KDMonCat.App.Input }
  , roleInfos    :: Array RoleInfo
  , types        :: Array { name :: String, typedef :: Typedef2 }
  , userId       :: String
  }

toProjectJS :: User -> Project -> ProjectJS
toProjectJS user { projectId, name, kdmoncats } =
  { projectId
  , name
  , nets: mempty
  , diagrams: mempty
  , kdmoncats: kdmoncats # foldMapWithIndex (\kdName input -> [{name: kdName, input}])
  , roleInfos: mempty
  , types: mempty
  , userId: user.uid
  }

fromProjectJS :: ProjectJS -> Project
fromProjectJS { projectId, name, kdmoncats } =
  { projectId
  , name
  , nets: mempty
  , diagrams: mempty
  , kdmoncats: kdmoncats # map (\{name: kdName, input} -> kdName /\ input) # fromFoldable
  , roleInfos: mempty
  , types: mempty
  }
