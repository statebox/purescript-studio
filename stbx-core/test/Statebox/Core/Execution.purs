module Test.Statebox.Core.Execution where

import Prelude

import Data.Array (length, (..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Debug.Trace (spy)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Statebox.Core.Execution
import Statebox.Core.Execution as Stbx
import Statebox.Core.Types (PID, TID, Wiring, GluedTransitionId(..))

-- | We define this FFI value in order to load the FFI module, which imports (requires) stbx.js.
foreign import requireStbxJs_HACK :: String

suite :: Spec Unit
suite = do
  describe "Protocol executions" do
    let
      -- | ```
      -- | a = 010100
      -- |
      -- |   = ,1,1,.                      { interpret 0 as a separator and terminator, digits as place ids }
      -- |
      -- |   = [],[1],[1],[].              { [] encodes empty sets of pre- and post places }
      -- |     \____/ \____/
      -- |      t         u
      -- |      _         _
      -- |   = [_]->(1)->[_]               { t,u are transitions labeled by the 'names' field }
      -- |      t         u
      -- | ```
      -- |
      -- | We label this net as 'n' and 'm', and glue it together into n&m:
      -- |
      -- | ```
      -- |      m =             n =
      -- |      _       _       _       _
      -- |     |_|-(1)-|_|     |_|-(1)-|_|
      -- |      t       u       t       u
      -- |
      -- |      |   |   \_______/   |   |
      -- |      |   |       |       |   |
      -- |      V   V       V       V   V
      -- |      _       _________       _
      -- |     |_|-(1)-|_________|-(1)-|_|
      -- |     m.t m.1     u&t     n.1 n.u
      -- | ```
      wiring :: Wiring
      wiring =
        { nets: [ { name: "a"
                  , partition: [0,1,0,1,0,0]
                  , names: ["t","u"]
                  -- , placeNames: Nothing
                  }
                ]
        , diagrams: [ { name: "z"
                      , width: 1
                      , pixels: [1,2]
                      , names: ["m","n"]
                      }
                    ]
        , labels: [0,0]
        }

      s0                 = spy "s0"              $ Stbx.fromWiring wiring
      transitions        = spy "transitions"     $ Stbx.gluedTransitions s0
      transitionCount    = spy "transitionCount" $ Stbx.transitionCount s0
      transitionIds      = spy "transitionIds"   $ Stbx.transitionIds s0

    it "should interpret glued net topology correctly" do
      let
        gluedTransition0 = spy "transition 0" $ Stbx.getGluedTransition s0 (GluedTransitionId 0)
        gluedTransition1 = spy "transition 1" $ Stbx.getGluedTransition s0 (GluedTransitionId 1)
        gluedTransition2 = spy "transition 2" $ Stbx.getGluedTransition s0 (GluedTransitionId 2)

      transitionCount `shouldEqual` 3
      transitionIds   `shouldEqual` [ GluedTransitionId 0, GluedTransitionId 1, GluedTransitionId 2]

      (gluedTransition0 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Initial
      (gluedTransition1 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Glued
      (gluedTransition2 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Final

    it "should interpret further glued net topology correctly" do
      let
        firing0 = spy "firing 0" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 0)
        firing1 = spy "firing 1" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 1)
        firing2 = spy "firing 2" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 2)

      firing2 `shouldEqual` Just { pre: [ [ 1 ], []    ], post: "u"   }
      firing1 `shouldEqual` Just { pre: [ [ 1 ], [ 1 ] ], post: "u&t" }
      firing0 `shouldEqual` Just { pre: [ []   , [ 1 ] ], post: "t"   }

    it "should be well-behaved at non-existing transitions" do
      let
        err1               = spy "enabledMaybe 666"   $ Stbx.enabledMaybe s0 666
        err2               = spy "enabled 666"        $ Stbx.enabled      s0 666

      err1 `shouldEqual` Nothing
      err2 `shouldEqual` false

    it "initial net state should be interpreted correctly" do
      let
        enabled0           = spy "enabled0"        $ Stbx.enabled            s0 0
        enabled0M          = spy "enabled0M"       $ Stbx.enabledMaybe       s0 0
        enabledTrsMaybe0   = spy "enabledTrs"      $ Stbx.enabledMaybe_glued s0 <$> transitionIds

        marking0           = spy "marking 0"       $ Stbx.marking s0

      marking0         `shouldEqual` []
      enabledTrsMaybe0 `shouldEqual` [Just true, Just false, Just false]
      enabled0M        `shouldEqual` (Just true)

    it "net state after firing transition 0" do
      let
        marked1            = spy "marked1 after fire" $ Stbx.fire s0 0
        marking1           = spy "marking 1"          $ Stbx.marking s1
        s1Maybe            = spy "s0Maybe after fire" $ Stbx.fromMarked <$> marked1
        s1                 = spy "s1      after fire" $ fromMaybe s0 s1Maybe
        enabledTrsMaybe1   = spy "enabledTrs s1"      $ Stbx.enabledMaybe_glued s1 <$> transitionIds

      marking1         `shouldEqual` [{ path: [0], marking: [1] }]
      enabledTrsMaybe1 `shouldEqual` [Just false, Just true, Just false]

  describe "Already glued net protocol executions" do
    let
      -- | ```
      -- | a = 0101020200
      -- |
      -- |   = ,1,1,2,2,.                  { interpret 0 as a separator and terminator, digits as place ids }
      -- |
      -- |   = [],[1],[1],[2],[2],[].      { [] encodes empty sets of pre- and post places }
      -- |     \____/ \____/  \____/
      -- |      t       u         v
      -- |      _              _
      -- |   = [_]->(1)->(2)->[_]          { t,u,v,w are transitions labeled by the 'names' field }
      -- |         t    u    v
      -- | ```
      wiring :: Wiring
      wiring =
        { nets: [ { name: "a"
                  , partition: [0,1,0,1,0,2,0,2,0,0]
                  , names: ["t","u","v"]
                  -- , placeNames: Nothing
                  }
                ]
        , diagrams: [ { name: "z"
                      , width: 1
                      , pixels: [1]
                      , names: ["m"]
                      }
                    ]
        , labels: [0]
        }

      s0                 = spy "s0"              $ Stbx.fromWiring wiring
      transitions        = spy "transitions"     $ Stbx.gluedTransitions s0
      transitionCount    = spy "transitionCount" $ Stbx.transitionCount s0
      transitionIds      = spy "transitionIds"   $ Stbx.transitionIds s0

    it "should interpret glued net topology correctly" do
      let
        gluedTransition0 = spy "transition 0" $ Stbx.getGluedTransition s0 (GluedTransitionId 0)
        gluedTransition1 = spy "transition 1" $ Stbx.getGluedTransition s0 (GluedTransitionId 1)
        gluedTransition2 = spy "transition 2" $ Stbx.getGluedTransition s0 (GluedTransitionId 2)

      transitionCount `shouldEqual` 3
      transitionIds   `shouldEqual` [ GluedTransitionId 0, GluedTransitionId 1, GluedTransitionId 2]

      (gluedTransition0 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Initial
      (gluedTransition1 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Normal
      (gluedTransition2 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Final

    it "should interpret further glued net topology correctly" do
      let
        firing0 = spy "firing 0" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 0)
        firing1 = spy "firing 1" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 1)
        firing2 = spy "firing 2" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 2)

      firing2 `shouldEqual` Just { pre: [ [ 2 ], []    ], post: "v" }
      firing1 `shouldEqual` Just { pre: [ [ 1 ], [ 2 ] ], post: "u" }
      firing0 `shouldEqual` Just { pre: [ []   , [ 1 ] ], post: "t" }

    it "should be well-behaved at non-existing transitions" do
      let
        err1               = spy "enabledMaybe 666"   $ Stbx.enabledMaybe s0 666
        err2               = spy "enabled 666"        $ Stbx.enabled      s0 666

      err1 `shouldEqual` Nothing
      err2 `shouldEqual` false

    it "initial net state should be interpreted correctly" do
      let
        enabled0           = spy "enabled0"        $ Stbx.enabled            s0 0
        enabled0M          = spy "enabled0M"       $ Stbx.enabledMaybe       s0 0
        enabledTrsMaybe0   = spy "enabledTrs"      $ Stbx.enabledMaybe_glued s0 <$> transitionIds

        marking0           = spy "marking 0"       $ Stbx.marking s0

      marking0         `shouldEqual` []
      enabledTrsMaybe0 `shouldEqual` [Just true, Just false, Just false]
      enabled0M        `shouldEqual` (Just true)

    it "net state after firing transition 0" do
      let
        marked1            = spy "marked1 after fire" $ Stbx.fire s0 0
        marking1           = spy "marking 1"          $ Stbx.marking s1
        s1Maybe            = spy "s0Maybe after fire" $ Stbx.fromMarked <$> marked1
        s1                 = spy "s1      after fire" $ fromMaybe s0 s1Maybe
        enabledTrsMaybe1   = spy "enabledTrs s1"      $ Stbx.enabledMaybe_glued s1 <$> transitionIds

      marking1         `shouldEqual` [{ path: [0], marking: [1] }]
      enabledTrsMaybe1 `shouldEqual` [Just false, Just true, Just false]

  describe "More complicated diagram protocol execution" do
    let
      -- | ```
      -- | a = 010100                              https://mermaidjs.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggTFJcblxucDEoKF8pKVxucDIoKDEpKVxucDMoKF8pKVxuXG5wMSAtLT4gdDFcbnQxLS0-IHAyXG5cbnAyIC0tPiB0MlxudDIgLS0-IHAzXG5cbnN0eWxlIHAxIGZpbGw6d2hpdGUsIHN0cm9rZTpncmV5XG5zdHlsZSBwMiBmaWxsOndoaXRlLCBzdHJva2U6Z3JleVxuc3R5bGUgcDMgZmlsbDp3aGl0ZSwgc3Ryb2tlOmdyZXlcblxuc3R5bGUgdDEgZmlsbDpjeWFuXG5zdHlsZSB0MiBmaWxsOmN5YW4iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9fQ
      -- |   |
      -- |   = ,1,1,.                              { interpret 0 as a separator and terminator, digits as place ids }
      -- |   |
      -- |   = [],[1],[1],[].                      { [] encodes empty sets of pre- and post places }
      -- |   | \____/ \____/
      -- |   |  u         v
      -- |   |  _         _
      -- |   = [_]->(1)->[_]                       { t,u are transitions labeled by the 'names' field }
      -- |      u         v
      -- |
      -- | b = 01101020120330300                   https://mermaidjs.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggTFJcblxucDEoKF8pKVxucDIoKDEpKVxucDMoKDIpKVxucDQoKDMpKVxucDUoKF8pKVxuXG5wMSAtLT4gdDFcbnQxIC0tIDIgLS0-IHAyXG5cbnAyIC0tPiB0MlxudDIgLS0-IHAzXG5cbnAyIC0tPiB0M1xucDMgLS0-IHQzXG50MyAtLSAyIC0tPiBwNFxuXG5wNCAtLT4gdDRcbnQ0IC0tPiBwNVxuXG5zdHlsZSBwMSBmaWxsOndoaXRlLCBzdHJva2U6Z3JleVxuc3R5bGUgcDIgZmlsbDp3aGl0ZSwgc3Ryb2tlOmdyZXlcbnN0eWxlIHAzIGZpbGw6d2hpdGUsIHN0cm9rZTpncmV5XG5zdHlsZSBwNCBmaWxsOndoaXRlLCBzdHJva2U6Z3JleVxuc3R5bGUgcDUgZmlsbDp3aGl0ZSwgc3Ryb2tlOmdyZXlcblxuc3R5bGUgdDEgZmlsbDpjeWFuXG5zdHlsZSB0MiBmaWxsOmN5YW5cbnN0eWxlIHQzIGZpbGw6Y3lhblxuc3R5bGUgdDQgZmlsbDpjeWFuIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQifX0
      -- |   |
      -- |   = ,11,1,2,12,33,3,.
      -- |   |
      -- |   = [],[11],[1],[2],[12],[33],[3],[].
      -- |     \_____/ \_____/ \_______/ \____/
      -- |      f        g         h         i
      -- |
      -- | c = 010010100                           https://mermaidjs.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggTFJcblxucDEoKF8pKVxucDIoKF8pKVxucDMoKDEpKVxucDQoKF8pKVxuXG5wMSAtLT4gdDFcbnQxIC0tPiBwM1xuXG5wMiAtLT4gdDJcbnQyIC0tPiBwM1xuXG5wMyAtLT4gdDNcbnQzIC0tPiBwNFxuXG5zdHlsZSBwMSBmaWxsOndoaXRlLCBzdHJva2U6Z3JleVxuc3R5bGUgcDIgZmlsbDp3aGl0ZSwgc3Ryb2tlOmdyZXlcbnN0eWxlIHAzIGZpbGw6d2hpdGUsIHN0cm9rZTpncmV5XG5zdHlsZSBwNCBmaWxsOndoaXRlLCBzdHJva2U6Z3JleVxuXG5zdHlsZSB0MSBmaWxsOmN5YW5cbnN0eWxlIHQyIGZpbGw6Y3lhblxuc3R5bGUgdDMgZmlsbDpjeWFuIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQifX0
      -- |   |
      -- |   = ,1,,1,1,.
      -- |   |
      -- |   = [],[1],[],[1],[1],[]
      -- |     \____/ \____/ \____/
      -- |      l       m      n
      -- |
      -- | y = 21233  (root diagram)               https://statebox-brick-diagrams.netlify.com/#!/21233
      -- |   |
      -- |   = 1 --┐  (replaced by the glueing of a and a via z)
      -- |         |
      -- |         v
      -- |         3  (replaced by c)
      -- |         ^
      -- |         |
      -- |     2 --┘  (replaces by b)
      -- |
      -- | z = 112                                 https://statebox-brick-diagrams.netlify.com/#!/112
      -- |   |
      -- |   = 1 -> 2 (both 1 and 2 gets replaced by a)
      -- |
      -- | the net resulting from the glueing is the following, where
      -- | - (1_a) indicates the place 1 in the a net
      -- | - the ' in used to differentiates two copies of the same net
      -- | - the numbers near the arrows indicate the transaction id asseigned to the glued net
      -- |
      -- |  _  0        2         5        7   _
      -- | |_| -> (1_a) -> (1_a') -> (1_c) -> |_|
      -- |                                 ^
      -- |  _  1        3                  |
      -- | |_| -> (1_b) -> (2_b)           |
      -- |          |        |             | 6
      -- |          └--------└-> (3_b) ----┘
      -- |              4
      -- | ```
      wiring :: Wiring
      wiring =
        { nets: [ { name: "a"
                  , partition: [0,1,0,1,0,0]
                  , names: ["u","v"]
                  }
                , { name: "b"
                  , partition: [0,1,1,0,1,0,2,0,1,2,0,3,3,0,3,0,0]
                  , names: ["f","g","h","i"]
                  }
                , { name: "c"
                  , partition: [0,1,0,0,1,0,1,0,0]
                  , names: ["l", "m", "n"]
                  }
                ]
        , diagrams: [ { name: "y"
                      , width: 2
                      , pixels : [1,2,3,3]
                      , names: ["p", "q", "r"]
                      }
                    , { name: "z"
                      , width: 1
                      , pixels: [1,2]
                      , names: ["s","t"]
                      }
                    ]
        , labels: [4,1,2,0,0]
        }

      s0                 = spy "s0"              $ Stbx.fromWiring wiring
      transitions        = spy "transitions"     $ Stbx.gluedTransitions s0
      transitionCount    = spy "transitionCount" $ Stbx.transitionCount s0
      transitionIds      = spy "transitionIds"   $ Stbx.transitionIds s0

    it "should interpret glued net topology correctly" do
      let
        gluedTransition0 = spy "transition 0" $ Stbx.getGluedTransition s0 (GluedTransitionId 0)
        gluedTransition1 = spy "transition 1" $ Stbx.getGluedTransition s0 (GluedTransitionId 1)
        gluedTransition2 = spy "transition 2" $ Stbx.getGluedTransition s0 (GluedTransitionId 2)
        gluedTransition3 = spy "transition 3" $ Stbx.getGluedTransition s0 (GluedTransitionId 3)
        gluedTransition4 = spy "transition 4" $ Stbx.getGluedTransition s0 (GluedTransitionId 4)
        gluedTransition5 = spy "transition 5" $ Stbx.getGluedTransition s0 (GluedTransitionId 5)
        gluedTransition6 = spy "transition 6" $ Stbx.getGluedTransition s0 (GluedTransitionId 6)
        gluedTransition7 = spy "transition 7" $ Stbx.getGluedTransition s0 (GluedTransitionId 7)

      transitionCount `shouldEqual` 8
      transitionIds   `shouldEqual` [ GluedTransitionId 0
                                    , GluedTransitionId 1
                                    , GluedTransitionId 2
                                    , GluedTransitionId 3
                                    , GluedTransitionId 4
                                    , GluedTransitionId 5
                                    , GluedTransitionId 6
                                    , GluedTransitionId 7
                                    ]

      (gluedTransition0 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Initial
      (gluedTransition1 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Initial
      (gluedTransition2 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Glued
      (gluedTransition3 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Normal
      (gluedTransition4 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Normal
      (gluedTransition5 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Glued
      (gluedTransition6 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Glued
      (gluedTransition7 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Final

    it "should interpret further glued net topology correctly" do
      let
        firing0 = spy "firing 0" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 0)
        firing1 = spy "firing 1" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 1)
        -- firing2 = spy "firing 2" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 2)
        firing3 = spy "firing 3" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 3)
        firing4 = spy "firing 4" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 4)
        -- firing5 = spy "firing 5" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 5)
        firing6 = spy "firing 6" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 6)
        firing7 = spy "firing 7" $ fromGluedTransition2JS <$> Stbx.getFiring s0 (GluedTransitionId 7)

      firing7 `shouldEqual` Just { pre: [ [ 6 ]   , []      ], post: "n"   }
      firing6 `shouldEqual` Just { pre: [ [ 5 ]   , [ 6 ]   ], post: "i&m" }
      firing4 `shouldEqual` Just { pre: [ [ 3, 4 ], [ 5, 5 ]], post: "h"   }
      firing3 `shouldEqual` Just { pre: [ [ 3 ]   , [ 4 ]   ], post: "g"   }
      firing1 `shouldEqual` Just { pre: [ []      , [ 3, 3 ]], post: "f"   }
      firing0 `shouldEqual` Just { pre: [ []      , [ 1    ]], post: "u"   }

    it "should be well-behaved at non-existing transitions" do
      let
        err1               = spy "enabledMaybe 666"   $ Stbx.enabledMaybe s0 666
        err2               = spy "enabled 666"        $ Stbx.enabled      s0 666

      err1 `shouldEqual` Nothing
      err2 `shouldEqual` false

    it "initial net state should be interpreted correctly" do
      let
        enabled0           = spy "enabled0"        $ Stbx.enabled            s0 0
        enabled0M          = spy "enabled0M"       $ Stbx.enabledMaybe       s0 0
        enabledTrsMaybe0   = spy "enabledTrs"      $ Stbx.enabledMaybe_glued s0 <$> transitionIds

        marking0           = spy "marking 0"       $ Stbx.marking s0

      marking0         `shouldEqual` []
      enabledTrsMaybe0 `shouldEqual` [ Just true
                                     , Just true
                                     , Nothing
                                     , Just false
                                     , Just false
                                     , Nothing
                                     , Just false
                                     , Just false
                                     ]
      enabled0M        `shouldEqual` (Just true)

    it "net state after firing transition 0" do
      let
        marked1            = spy "marked1 after fire" $ Stbx.fire s0 0
        s1Maybe            = spy "s0Maybe after fire" $ Stbx.fromMarked <$> marked1
        s1                 = spy "s1      after fire" $ fromMaybe s0 s1Maybe
        marking1           = spy "marking 1"          $ Stbx.marking s1
        enabledTrsMaybe1   = spy "enabledTrs s1"      $ Stbx.enabledMaybe_glued s1 <$> transitionIds

      marking1         `shouldEqual` [{ path: [0, 0], marking: [1] }]
      enabledTrsMaybe1 `shouldEqual` [ Just false
                                     , Just false
                                     , Nothing
                                     , Just false
                                     , Just false
                                     , Nothing
                                     , Just false
                                     , Just false
                                     ]
