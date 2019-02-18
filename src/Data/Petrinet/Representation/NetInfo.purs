module Data.Petrinet.Representation.NetInfo where

import Data.Monoid
import Data.Petrinet.Representation.Dict
import Data.Petrinet.Representation.NLL
import View.Petrinet.Model

import Data.Auth (Role, Roles, RoleInfo)
import Data.Array ((..), length)
import Data.Map (Map)
import Data.Set
import Data.Tuple.Nested (type (/\), (/\), Tuple2)
import Data.Tuple (fst)
import Data.Vec3 (Vec2, Vec2D)
import Prelude ((<<<), identity, map, show, const, ($))
import Data.Maybe (Maybe)

class PointLabel point where
  labelForPoint :: point -> Maybe String

class TransLabel trans where
  labelForTrans :: trans -> Maybe String



mkNetInfo :: String -> NetObj -> NetApi -> Array TextBox -> NetInfo
mkNetInfo name net netApi textBoxes = { name, net, netApi, textBoxes}

parseNetF :: NetF Int -> NetInfo
parseNetF netF = mkNetInfo ""  (parseNetObj netF) (parseNetApi netF) (parseTextBoxes netF)

parseNetObj :: NetF Int -> NetObj
parseNetObj = mkNetObjF <<< getNetRep
  where
    markingMap :: NetF Int -> Map Int Int
    markingMap netf = mempty
    parseMarking :: NetF Int -> Marking
    parseMarking = mkMarkingF <<< markingMap
    getNetRep :: NetF Int -> NetRep
    getNetRep netF = mkNetRep (mkArrayPID netF) 
                              (mkArrayTrans netF) 
                              (parseMarking netF) 
                              (mkArrayPIDString netF)
                              (mkArrayPIDVec netF) 
                              (mkArrayLabel netF) 
                              (mkArrayTypedef netF) 
                              (mkTransLocation netF)
                              (mkArrayRoles netF)
    mkArrayPID :: NetF Int -> Array PID
    mkArrayPID = map fst <<< mkArrayPIDString
    mkArrayTrans :: NetF Int -> Array Transition
    mkArrayTrans = map setPairToTransition 
    mkArrayPIDString :: NetF Int -> Array (PID /\ String)
    mkArrayPIDString netF = []
    mkArrayPIDVec :: NetF Int -> Array (PID /\ Vec2D)
    mkArrayPIDVec net = [] -- HOW
    mkArrayLabel :: NetF Int -> Array String
    mkArrayLabel net = map (\i -> "T" <> show i) [0 .. (length net)]
    mkArrayTypedef :: NetF Int -> Array Typedef
    mkArrayTypedef net = map (const $ Typedef "1") net
    mkTransLocation :: NetF Int -> Array Vec2D
    mkTransLocation net = [] -- The position of all transitions? how?
    mkArrayRoles :: NetF Int -> Array Roles
    mkArrayRoles net = []
    setPairToTransition :: Array Int /\ Array Int -> Transition
    setPairToTransition (src /\ trg) = { pre: map { place: _, tokens: 0 } src
                                       , post: map { place: _, tokens: 0 } trg
                                       }
    getAllPoints :: Eq a => NetF a -> Set a
    getAllPoints = foldl (\set pair -> ?unsure) mempty
    

parseNetApi :: ∀ a. NetF a -> NetApi
parseNetApi netF = { findTokens : identity }

parseTextBoxes :: ∀ a. NetF a -> Array TextBox
parseTextBoxes netF = []
