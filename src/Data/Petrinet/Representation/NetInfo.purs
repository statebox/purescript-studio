module Data.Petrinet.Representation.NetInfo where

import Data.Petrinet.Representation.Dict
import Data.Petrinet.Representation.NLL
import Data.Map (Map)
import Data.Monoid
import View.Petrinet.Model
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
    getNetRep netF = mkNetRep [] [] (parseMarking netF) [] [] [] [] [] []


parseNetApi :: ∀ a. NetF a -> NetApi
parseNetApi netF = { findTokens : identity }

parseTextBoxes :: ∀ a. NetF a -> Array TextBox
parseTextBoxes netF = []
