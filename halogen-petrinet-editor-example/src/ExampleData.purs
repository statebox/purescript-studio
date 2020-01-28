module ExampleData where

import Prelude
import Data.Array ((..), length)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un, over)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Ring
import Data.Vec3.Box (Box(..))
import Data.Vec3 (Vec2D, Vec2(..), vec2, vec3, _x, _y, _z)

import Data.Auth (Role(..), Roles(..), Privilege(..), rolesFromFoldable, CSSColor(..))
import Data.Petrinet.Representation.Marking as Marking
import Data.Typedef (Typedef(..))
import Data.Typedef.Typedef2 (Typedef2(..))
import View.Petrinet.Model (PID, TID, Tokens, Transition, Marking, PlaceMarking, NetRep, mkNetRepUsingLayout, mkNetApi, NetApi, NetInfo, NetInfoFRow, NetLayout, TextBox)

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
marking1 = Marking.fromFoldable
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

transitionRoles1 = rolesFromFoldable <$>
  [ []
  , [ Role 1 ]
  , []
  , []
  ]

layout1 :: NetLayout
layout1 =
  { placePointsDict: Map.fromFoldable
      [ 1 /\ vec2 10.0 30.0
      , 2 /\ vec2 30.0 30.0
      , 3 /\ vec2 90.0 30.0
      , 4 /\ vec2 70.0 30.0
      , 5 /\ vec2 50.0 30.0
      ]
  , transitionPointsDict: Map.fromFoldable
      [ 6 /\ vec2 30.0 20.0
      , 7 /\ vec2 30.0 40.0
      , 8 /\ vec2 70.0 20.0
      , 9 /\ vec2 70.0 40.0
      ]
  , edgeWaypointsDict: Map.empty
  }

textBoxes1 :: Array TextBox
textBoxes1 = []

net1 :: NetRep
net1 = mkNetRepUsingLayout places1 transitions1 marking1 placeLabels1 transitionLabels1 (pure layout1) transitionTypes1 transitionRoles1

netInfo1 :: NetInfo
netInfo1 = { name: "Traffic lights", net: net1, netApi: mkNetApi net1, textBoxes: textBoxes1 }

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
marking2 = Marking.fromFoldable
  [ 1 /\ 1
  , 5 /\ 1
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

transitionRoles2 = rolesFromFoldable <$>
  [ []
  , [ Role 1 ]
  , [ Role 2 ]
  , []
  ]

layout2 :: NetLayout
layout2 =
  { placePointsDict: Map.fromFoldable
      [ 1 /\ vec2 10.0 top2
      , 2 /\ vec2 10.0 bot2
      , 3 /\ vec2 50.0 mid2
      , 4 /\ vec2 90.0 top2
      , 5 /\ vec2 90.0 bot2
      ]
  , transitionPointsDict: Map.fromFoldable
      [ 6 /\ vec2 (-10.0) mid2
      , 7 /\ vec2   30.0  mid2
      , 8 /\ vec2   70.0  mid2
      , 9 /\ vec2  110.0  mid2
      ]
  , edgeWaypointsDict: Map.empty
  }
  where
    top2 = 10.0
    mid2 = 30.0
    bot2 = 50.0

textBoxes2 :: Array TextBox
textBoxes2 =
  [ { name: "producer", text: "Producer", box: Box { topLeft: vec2 (-14.0) 6.0, bottomRight: vec2  38.0 56.0 } }
  , { name: "consumer", text: "Consumer", box: Box { topLeft: vec2   60.0  6.0, bottomRight: vec2 115.0 56.0 } }
  ]

net2 :: NetRep
net2 = mkNetRepUsingLayout places2 transitions2 marking2 placeLabels2 transitionLabels2 (pure layout2) transitionTypes2 transitionRoles2

netInfo2 :: NetInfo
netInfo2 = { name: "Producer-consumer", net: net2, netApi: mkNetApi net2, textBoxes: textBoxes2 }
