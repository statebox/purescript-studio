module ExampleData
  ( projects
  , project1
  , project2
  , net1
  , netApi1
  , net2
  , netApi2
  , pnproNetInfos1
  , diagrams
  ) where

import Prelude
import Data.Array ((..), length)
import Data.Bag as Bag
import Data.Bag (BagF(..))
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un, over)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Ring
import Data.Vec2D (Vec2D, Vec2(..), vec2, Box(..))

import Data.Auth (Role(..), Roles(..), Privilege(..), rolesFromFoldable, CSSColor(..))
import Data.Petrinet.Representation.Dict
import Data.Petrinet.Representation.PNPRO as PNPRO
import Data.Typedef.Typedef2 (Typedef2(..))
import View.Model (Project)
import View.Petrinet.Model (PID, TID, Tokens, Typedef(..), Transition, Marking, PlaceMarking, NetRep, mkNetRep, NetObj, NetApi, NetInfo, NetInfoFRow, TextBox)
import View.Diagram.Model (DiagramInfo)

projects :: Array Project
projects = [project1, project2]

project1 :: Project
project1 =
  { name: "Statebox Examples"
  , nets: [ netInfo1, netInfo2 ]
  , diagrams: diagrams
  , roleInfos: project1Roles
  , types: project1Typedefs
  }

project1Roles =
  [ { id: Role 0, name: "admin"   , bgColor: CSSColor "orange", textColor: CSSColor "white" }
  , { id: Role 1, name: "producer", bgColor: CSSColor "purple", textColor: CSSColor "white" }
  , { id: Role 2, name: "consumer", bgColor: CSSColor "pink"  , textColor: CSSColor "white" }
  ]

project1Typedefs =
  [ "Message"      /\ TProd [person, person, TRef "Date", TRef "String"]
  , "TrafficLight" /\ TSum  [TUnit, TUnit, TUnit]
  , "Date"         /\ TProd [TRef "Int", TRef "Int", TRef "Int"]
  , "Int"          /\ TSum  (TRef <$> ["Bit", "Bit", "Bit", "Bit", "Bit", "Bit", "Bit", "Bit" ])
  , "Bit"          /\ TSum  [TUnit, TUnit]
  ]
  where
    person = TProd [TRef "String", TRef "Date"]

--------------------------------------------------------------------------------

project2 :: Project
project2 =
  { name: "Erik's examples"
  , nets: pnproNetInfos1
  , diagrams: diagrams
  , roleInfos: project2Roles
  , types: project2Typedefs
  }

project2Roles =
  [ { id: Role 0, name: "admin"   , bgColor: CSSColor "orange", textColor: CSSColor "white" }
  , { id: Role 1, name: "player 1", bgColor: CSSColor "purple", textColor: CSSColor "white" }
  , { id: Role 2, name: "player 2", bgColor: CSSColor "pink"  , textColor: CSSColor "white" }
  ]

project2Typedefs :: Array (String /\ Typedef2)
project2Typedefs =
  [ "ItemId"            /\ TRef  "Int"
  , "RockPaperScissors" /\ TProd [TRef "String", TSum  [TUnit, TUnit, TProd [TRef "Int", TRef "Int", TRef "Int"]]]
  , "Date"              /\ TProd [TRef "Int", TRef "Int", TRef "Int"]
  , "Int"               /\ TSum  (TRef <$> ["Bit", "Bit", "Bit", "Bit", "Bit", "Bit", "Bit", "Bit" ])
  , "Bit"               /\ TSum  [TUnit, TUnit]
  ]

-- traffic lights net ----------------------------------------------------------

places1 = 1 .. numPlaces1
  where
    numPlaces1 = length placeLabels1

placeLabels1 :: Array (PID /\ String)
placeLabels1 =
  [ 1 /\ "green_1"
  , 2 /\ "red_1"
  , 3 /\ "green_2"
  , 4 /\ "red_2"
  , 5 /\ "queue"
  ]

marking1 :: Marking
marking1 = Bag.fromFoldable
  [ 1 /\ 1
  , 4 /\ 1
  ]

placePoints1 :: Array (PID /\ Vec2D)
placePoints1 =
  [ 1 /\ vec2 10.0 30.0
  , 2 /\ vec2 30.0 30.0
  , 3 /\ vec2 90.0 30.0
  , 4 /\ vec2 70.0 30.0
  , 5 /\ vec2 50.0 30.0
  ]

--------------------------------------------------------------------------------

transitions1 :: Array Transition
transitions1 =
  [ { pre:  ms [ m 1 1 ] -- a
    , post: ms [ m 2 1   -- b
               , m 5 1   -- queue
               ]
    }
  , { pre:  ms [ m 2 1   -- b
               , m 5 1   -- queue
               ]
    , post: ms [ m 1 1 ] -- a
    }
  , { pre:  ms [ m 4 1   -- d
               , m 5 1   -- queue
               ]
    , post: ms [ m 3 1 ] -- c
    }
  , { pre:  ms [ m 3 1 ] -- c
    , post: ms [ m 4 1   -- d
               , m 5 1   -- queue
               ]
    }
  ]
  where
    m p tok = { place: p, tokens: tok }
    ms      = identity

transitionLabels1 :: Array String
transitionLabels1 =
  [ "T1"
  , "T2"
  , "T3"
  , "T4"
  ]

transitionTypes1 :: Array Typedef
transitionTypes1 =
  [ Typedef "1"
  , Typedef "(* 1 1)"
  , Typedef "(+ 1 1)"
  , Typedef "(* 1 (* 0 1))"
  ]

transitionPoints1 =
  [ vec2 30.0 20.0
  , vec2 30.0 40.0
  , vec2 70.0 20.0
  , vec2 70.0 40.0
  ]

transitionRoles1 = rolesFromFoldable <$>
  [ []
  , [ Role 1 ]
  , []
  , []
  ]

--------------------------------------------------------------------------------

textBoxes1 :: Array TextBox
textBoxes1 = []

net1Data :: NetRep
net1Data = mkNetRep places1 transitions1 marking1 placeLabels1 placePoints1 transitionLabels1 transitionTypes1 transitionPoints1 transitionRoles1

net1 :: NetObj
net1 = mkNetObjF net1Data

netApi1 :: NetApi
netApi1 =
  { findTokens : findTokens' net1Data.marking
  }

netInfo1 :: NetInfo
netInfo1 = { name: "Traffic lights", net: net1, netApi: netApi1, textBoxes: textBoxes1 }

-- producer-consumer net -------------------------------------------------------

places2 = 1 .. numPlaces2
  where
    numPlaces2 = length placeLabels2

placeLabels2 :: Array (PID /\ String)
placeLabels2 =
  [ 1 /\ "P0"
  , 2 /\ "P1"
  , 3 /\ "P2"
  , 4 /\ "P3"
  , 5 /\ "P4"
  ]

marking2 :: Marking
marking2 = Bag.fromFoldable
  [ 1 /\ 1
  , 5 /\ 1
  ]

top2 = 10.0
mid2 = 30.0
bot2 = 50.0

placePoints2 :: Array (TID /\ Vec2D)
placePoints2 =
  [ 1 /\ vec2 10.0 top2
  , 2 /\ vec2 10.0 bot2
  , 3 /\ vec2 50.0 mid2
  , 4 /\ vec2 90.0 top2
  , 5 /\ vec2 90.0 bot2
  ]

transitions2 :: Array Transition
transitions2 =
  [ { pre:  ms [ m 2 1 ]
    , post: ms [ m 1 1 ]
    }
  , { pre:  ms [ m 1 1 ]
    , post: ms [ m 2 1
               , m 3 1
               ]
    }
  , { pre:  ms [ m 3 1
               , m 5 1
               ]
    , post: ms [ m 4 1 ]
    }
  , { pre:  ms [ m 4 1 ]
    , post: ms [ m 5 1 ]
    }
  ]
  where
    m p tok = { place: p, tokens: tok }
    ms      = identity

transitionTypes2 :: Array Typedef
transitionTypes2 =
  [ Typedef "1"
  , Typedef "(* 1 1)"
  , Typedef "(+ 1 1)"
  , Typedef "(* 1 (* 0 1))"
  ]

transitionLabels2 :: Array String
transitionLabels2 =
  [ "T1"
  , "T2"
  , "T3"
  , "T4"
  ]

transitionPoints2 =
  [ vec2 (-10.0) mid2
  , vec2   30.0  mid2
  , vec2   70.0  mid2
  , vec2  110.0  mid2
  ]

transitionRoles2 = rolesFromFoldable <$>
  [ []
  , [ Role 1 ]
  , [ Role 2 ]
  , []
  ]

textBoxes2 :: Array TextBox
textBoxes2 =
  [ { name: "Producer", box: Box { topLeft: vec2 (-14.0) 6.0, bottomRight: vec2  38.0 54.0 } }
  , { name: "Consumer", box: Box { topLeft: vec2   60.0  6.0, bottomRight: vec2 115.0 54.0 } }
  ]

net2Data :: NetRep
net2Data = mkNetRep places2 transitions2 marking2 placeLabels2 placePoints2 transitionLabels2 transitionTypes2 transitionPoints2 transitionRoles2

net2 :: NetObj
net2 = mkNetObjF net2Data

netApi2 :: NetApi
netApi2 =
  { findTokens : findTokens' net2Data.marking
  }

netInfo2 :: NetInfo
netInfo2 = { name: "Producer-consumer", net: net2, netApi: netApi2, textBoxes: textBoxes2 }

--------------------------------------------------------------------------------

pnproNetInfos1 :: Array NetInfo
pnproNetInfos1 = PNPRO.mkNetInfo <$> pnproProject1.project.gspn
  where
    pnproProject1 :: PNPRO.Document
    pnproProject1 = PNPRO.fromStringUnsafe pnproXml1

pnproXml1 =
  """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<project name="webshop" version="121">
  <gspn name="Enrico's Pizza Palazzo" show-color-cmd="false" show-fluid-cmd="false" show-timed-cmd="false" view-rates="false">
    <nodes>
      <place name="loggedIn" superposition-tags="Cart" x="12.0" y="16.0"/>
      <transition name="selectItem" nservers-x="0.5" type="EXP" x="7.55" y="9.0"/>
      <transition name="sendCart" nservers-x="0.5" superposition-tags="Cart" type="EXP" x="17.55" y="16.0"/>
      <place name="reviewCart" x="21.0" y="16.0"/>
      <transition name="checkout" nservers-x="0.5" superposition-tags="Unit" type="EXP" x="26.55" y="16.0"/>
      <transition name="startPayment" nservers-x="0.5" type="EXP" x="35.55" y="16.0"/>
      <transition name="selectPaymentMethod" nservers-x="0.5" type="EXP" x="45.55" y="16.0"/>
      <transition name="pay" nservers-x="0.5" type="EXP" x="55.55" y="16.0"/>
      <place name="reviewPayment" superposition-tags="Unit" x="31.0" y="16.0"/>
      <place name="paymentForm" superposition-tags="PaymentInfo" x="40.0" y="16.0"/>
      <place name="confirmPayment" superposition-tags="Unit" x="50.0" y="16.0"/>
      <transition name="login" nservers-x="0.5" type="EXP" x="7.55" y="16.0"/>
      <text-box bold="true" border-color="none" fill-color="none" height="2.5" name="__textBox0" shadow="true" shape="ROUND_RECTANGLE" text-color="#000000" vert-pos="0" width="19.0" x="1.5" y="24.75">Cart = (Qty \times ItemId)^*</text-box>
      <text-box bold="true" border-color="none" fill-color="none" height="3.0" name="__textBox1" shadow="true" shape="ROUND_RECTANGLE" text-color="#000000" vert-pos="0" width="27.5" x="23.25" y="24.5">PaymentInfo  = PaymentMethod \times Amount\\CustomerInfo = String \times PrivateKey </text-box>
      <place marking="1" name="loginForm" superposition-tags="Credentials" x="3.0" y="16.0"/>
      <place name="itemSelected" superposition-tags="ItemId" x="12.0" y="2.0"/>
      <transition name="addItem" nservers-x="0.5" type="EXP" x="17.55" y="9.0"/>
      <transition name="removeItem" nservers-x="0.5" type="EXP" x="12.55" y="9.0"/>
    </nodes>
    <edges>
      <arc head="reviewCart" kind="OUTPUT" tail="sendCart"/>
      <arc head="checkout" kind="INPUT" tail="reviewCart"/>
      <arc head="reviewPayment" kind="OUTPUT" tail="checkout"/>
      <arc head="startPayment" kind="INPUT" tail="reviewPayment"/>
      <arc head="paymentForm" kind="OUTPUT" tail="startPayment"/>
      <arc head="selectPaymentMethod" kind="INPUT" tail="paymentForm"/>
      <arc head="confirmPayment" kind="OUTPUT" tail="selectPaymentMethod"/>
      <arc head="pay" kind="INPUT" tail="confirmPayment"/>
      <arc head="loggedIn" kind="OUTPUT" tail="login"/>
      <arc head="sendCart" kind="INPUT" tail="loggedIn"/>
      <arc head="selectItem" kind="INPUT" tail="loggedIn">
        <point x="9.0" y="14.0"/>
      </arc>
      <arc head="login" kind="INPUT" tail="loginForm"/>
      <arc head="itemSelected" kind="OUTPUT" tail="selectItem"/>
      <arc head="addItem" kind="INPUT" tail="itemSelected"/>
      <arc head="loggedIn" kind="OUTPUT" tail="addItem">
        <point x="16.5" y="14.0"/>
      </arc>
      <arc head="removeItem" kind="INPUT" tail="itemSelected"/>
      <arc head="loggedIn" kind="OUTPUT" tail="removeItem"/>
    </edges>
  </gspn>
  <measures gspn-name="PT" name="Measures" simplified-UI="false">
    <assignments/>
    <greatspn/>
    <formulas>
      <formula comment="Basic statistics of the toolchain execution." language="STAT"/>
    </formulas>
  </measures>
</project>"""

--------------------------------------------------------------------------------

diagrams :: Array DiagramInfo
diagrams = [{ name: "Foo Bar Quux" }]
