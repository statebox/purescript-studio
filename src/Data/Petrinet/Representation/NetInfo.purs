module Data.Petrinet.Representation.NetInfo where

import Data.Monoid
import Data.Petrinet.Representation.Dict
import Data.Petrinet.Representation.NLL
import View.Petrinet.Model

import Data.Auth (Roles)
import Data.Array ((..), length)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Set (Set, fromFoldable, toUnfoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (fst, snd)
import Data.Vec3 (Vec2D, vec2)
import Prelude


mkNetInfo :: String -> NetObj -> NetApi -> Array TextBox -> NetInfo
mkNetInfo name net netApi textBoxes = { name, net, netApi, textBoxes }

parseNetF :: String -> (PID -> String) -> NetF Int -> NetInfo
parseNetF name placeNames netF = mkNetInfo name (parseNetObj netF placeNames) (parseNetApi netF) (parseTextBoxes netF)

-- | Takes a NetF and a mapping from places to String and return a NetObj
-- | The mapping is used to display the name of each place
-- | This function does not return the position of each place and transition properly
-- | This function generates names for transition of the form  "Ti" where "i" is an index
-- | This function does not return Roles properly
parseNetObj :: NetF Int -> (PID -> String) -> NetObj
parseNetObj netF placeNames = mkNetObjF getNetRep
  where
    -- What is a marking? How do I make this map?
    markingMap :: NetF Int -> Map Int Int
    markingMap netf = mempty
    parseMarking :: NetF Int -> Marking
    parseMarking = mkMarkingF <<< markingMap
    getNetRep :: NetRep
    getNetRep = mkNetRep (mkPlaces netF)
                         (mkTransitions netF)
                         (parseMarking netF)
                         (mkPlacesLabels netF)
                         (mkArrayPIDVec netF)
                         (mkTransLabels netF)
                         (mkTransTypes netF)
                         (mkTransLocation netF)
                         (mkArrayRoles netF)
    mkPlaces :: NetF Int -> Array PID
    mkPlaces = toUnfoldable <<< getAllPlacess
    mkTransitions :: NetF Int -> Array Transition
    mkTransitions = map setPairToTransition
    mkPlacesLabels :: NetF Int -> Array (PID /\ String)
    mkPlacesLabels = map (\id -> (id /\ placeNames id)) <<< mkPlaces
    -- This returns 0 vectors for now
    mkArrayPIDVec :: NetF PID -> Array (PID /\ Vec2D)
    mkArrayPIDVec = map (\id -> (id /\ vec2 (toNumber 0) (toNumber 0))) <<< mkPlaces
    mkTransLabels :: NetF TID -> Array String
    mkTransLabels net = map (\i -> "T" <> show i) [0 .. (length net)]
    -- Right now this barely does anything, what should be done?
    mkTransTypes :: NetF Int -> Array Typedef
    mkTransTypes net = map (const $ Typedef "1") net
    mkTransLocation :: NetF TID -> Array Vec2D
    mkTransLocation net = [] -- The position of all transitions? how?
    mkArrayRoles :: NetF Int -> Array Roles
    mkArrayRoles net = []
    setPairToTransition :: Array Int /\ Array Int -> Transition
    setPairToTransition (src /\ trg) = { pre: map { place: _, tokens: 0 } src
                                       , post: map { place: _, tokens: 0 } trg
                                       }
    getAllPlacess :: ∀ a. Ord a => Eq a => NetF a -> Set a
    getAllPlacess net = fromFoldable $ do transitions <- net
                                          fst transitions <> snd transitions

    

parseNetApi :: ∀ a. NetF a -> NetApi
parseNetApi netF = { findTokens : identity }

parseTextBoxes :: ∀ a. NetF a -> Array TextBox
parseTextBoxes netF = []
