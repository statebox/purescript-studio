module Data.Petrinet.Representation.NetInfo where

import Data.Monoid
import Data.Petrinet.Representation.Dict
import Data.Petrinet.Representation.NLL
import View.Petrinet.Model

import Data.Map (Map)
import Data.Auth (Role, Roles, RoleInfo)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (Vec2, Vec2D)
import Prelude ((<<<), identity)

mkNetInfo :: String -> NetObj -> NetApi -> Array TextBox -> NetInfo
mkNetInfo name net netApi textBoxes = { name, net, netApi, textBoxes}

parseNetF :: ∀ a. NetF a -> NetInfo
parseNetF netF = mkNetInfo ""  (parseNetObj netF) (parseNetApi netF) (parseTextBoxes netF)

parseNetObj :: ∀ a. NetF a -> NetObj
parseNetObj = mkNetObjF <<< getNetRep
  where
    markingMap :: NetF a -> Map Int Int
    markingMap netf = mempty
    parseMarking :: NetF a -> Marking
    parseMarking = mkMarkingF <<< markingMap
    getNetRep :: NetF a -> NetRep
    getNetRep netF = mkNetRep (mkArrayPID netF) 
                              (mkArrayTrans netF) 
                              (parseMarking netF) 
                              (mkArrayPIDString netF)
                              (mkArrayPIDVec netF) 
                              (mkArrayString netF) 
                              (mkArrayTypedef netF) 
                              (mkArrayVec2D netF)
                              (mkArrayRoles netF)
    mkArrayPID :: NetF a -> Array PID
    mkArrayPID netF = []
    mkArrayTrans :: NetF a -> Array Transition
    mkArrayTrans netF = []
    mkArrayPIDString :: NetF a -> Array (PID /\ String)
    mkArrayPIDString netF = []
    mkArrayPIDVec :: NetF a -> Array (PID /\ Vec2D)
    mkArrayPIDVec net = []
    mkArrayString :: NetF a -> Array String
    mkArrayString net = []
    mkArrayTypedef :: NetF a -> Array Typedef
    mkArrayTypedef net = []
    mkArrayVec2D :: NetF a -> Array Vec2D
    mkArrayVec2D net = []
    mkArrayRoles :: NetF a -> Array Roles
    mkArrayRoles net = []


parseNetApi :: ∀ a. NetF a -> NetApi
parseNetApi netF = { findTokens : identity }

parseTextBoxes :: ∀ a. NetF a -> Array TextBox
parseTextBoxes netF = []
