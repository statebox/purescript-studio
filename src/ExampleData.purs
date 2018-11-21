module ExampleData
  ( net1
  , netApi1
  , net2
  , netApi2
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
import Data.Vec2D (Vec2D)

import Data.Petrinet.Representation.Dict
import Model (PID, TID, Tokens, Typedef(..), Transition, Marking, PlaceMarking, NetRep, mkNetRep, NetObj, NetApi, NetInfo, NetInfoFRow)

-- traffic lights net ----------------------------------------------------------

places1 = 1 .. numPlaces1
  where
    numPlaces1 = length placeLabels1

placeLabels1 :: Array (PID /\ String)
placeLabels1 =
  [ 1 /\ "a"
  , 2 /\ "b"
  , 3 /\ "c"
  , 4 /\ "d"
  , 5 /\ "queue"
  ]

marking1 :: Marking
marking1 = Bag.fromFoldable
  [ 1 /\ 1
  , 4 /\ 1
  ]

placePoints1 :: Array (Int /\ Vec2D)
placePoints1 =
  [ 1 /\ { x: 10.0, y: 30.0 }
  , 2 /\ { x: 30.0, y: 30.0 }
  , 3 /\ { x: 90.0, y: 30.0 }
  , 4 /\ { x: 70.0, y: 30.0 }
  , 5 /\ { x: 50.0, y: 30.0 }
  ]

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
  [ { x: 30.0, y: 20.0 }
  , { x: 30.0, y: 40.0 }
  , { x: 70.0, y: 20.0 }
  , { x: 70.0, y: 40.0 }
  ]

net1Data :: NetRep
net1Data = mkNetRep places1 transitions1 marking1 placeLabels1 placePoints1 transitionLabels1  transitionTypes1 transitionPoints1

net1 :: NetObj
net1 = mkNetObjF net1Data

netApi1 :: NetApi
netApi1 =
  { findTokens : findTokens' net1Data.marking
  }

netInfo1 :: NetInfo
netInfo1 = { name: "Traffic lights", net: net1, netApi: netApi1 }

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

placePoints2 :: Array (Int /\ Vec2D)
placePoints2 =
  [ 1 /\ { x: 10.0, y: top2 }
  , 2 /\ { x: 10.0, y: bot2 }
  , 3 /\ { x: 50.0, y: mid2 }
  , 4 /\ { x: 90.0, y: top2 }
  , 5 /\ { x: 90.0, y: bot2 }
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
  [ { x: -10.0, y: mid2 }
  , { x:  30.0, y: mid2 }
  , { x:  70.0, y: mid2 }
  , { x: 110.0, y: mid2 }
  ]

net2Data :: NetRep
net2Data = mkNetRep places2 transitions2 marking2 placeLabels2 placePoints2 transitionLabels2 transitionTypes2 transitionPoints2

net2 :: NetObj
net2 = mkNetObjF net2Data

netApi2 :: NetApi
netApi2 =
  { findTokens : findTokens' net2Data.marking
  }

netInfo2 :: NetInfo
netInfo2 = { name: "Producer-consumer", net: net2, netApi: netApi2 }
