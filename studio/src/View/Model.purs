module View.Model where

import Prelude

import Data.Map (Map, fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.FoldableWithIndex
import Data.Lens (Lens')
import Data.Lens.Record
import Data.Symbol

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
  , kdmoncats    :: Map String KDMonCatData
  , roleInfos    :: Array RoleInfo
  , types        :: Array (String /\ Typedef2)
  }

type ProjectName = String
type ProjectId = String
type KDMonCatData = { name :: String, input :: KDMonCat.App.Input }

emptyProject :: Project
emptyProject = mempty

_kdmoncats :: Lens' Project (Map String KDMonCatData)
_kdmoncats = prop (SProxy :: SProxy "kdmoncats")

--------------------------------------------------------------------------------

type NetInfoWithTypesAndRoles = Record (NetInfoWithTypesAndRolesFRow PID TID Typedef Typedef2 ())

--------------------------------------------------------------------------------

type User =
  { email :: String
  , uid :: String
  , metadata ::
    { lastSignInTime :: String
    , creationTime :: String
    }
  }

type ProjectJS =
  { name         :: String
  , nets         :: Array NetInfo
  , diagrams     :: Array DiagramInfo
  , kdmoncats    :: Array { id :: String, name :: String, input :: KDMonCat.App.Input }
  , roleInfos    :: Array RoleInfo
  , types        :: Array { name :: String, typedef :: Typedef2 }
  , userId       :: String
  }

toProjectJS :: User -> Project -> ProjectJS
toProjectJS user { name, kdmoncats } =
  { name
  , nets: mempty
  , diagrams: mempty
  , kdmoncats: kdmoncats # foldMapWithIndex (\id {name: kdName, input} -> [{id, name: kdName, input}])
  , roleInfos: mempty
  , types: mempty
  , userId: user.uid
  }

fromProjectJS :: ProjectJS -> Project
fromProjectJS { name, kdmoncats } =
  { name
  , nets: mempty
  , diagrams: mempty
  , kdmoncats: kdmoncats # map (\{id, name: kdName, input} -> id /\ { name: kdName, input }) # fromFoldable
  , roleInfos: mempty
  , types: mempty
  }
