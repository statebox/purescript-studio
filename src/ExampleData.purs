module ExampleData
  ( PID
  , TID
  , Tokens
  , NetRep
  , NetApiF
  , net1
  , netApi1
  ) where

import Prelude
import Data.Array ((..), length)
import Data.Bag as Bag
import Data.FunctorWithIndex (mapWithIndex)
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

-- types specialised to Int index
type PID          = Int
type TID          = Int
type Tokens       = Int
type Transition   = TransitionF   PID Tokens
type Marking      = MarkingF      PID Tokens
type PlaceMarking = PlaceMarkingF PID Tokens

type NetApiF pid tid tok =
  { findTokens :: pid -> tok
  }

type NetRep = NetRepF PID TID Tokens

--------------------------------------------------------------------------------

-- TODO rm
places1 = 1 .. numPlaces1
  where
    numPlaces1 = length labels1

labels1 :: Array (PID /\ String)
labels1 =
  [ 1 /\ "a"
  , 2 /\ "b"
  , 3 /\ "c"
  , 4 /\ "d"
  , 5 /\ "queue"
  ]

labelsDict1 :: Map PID String
labelsDict1 = Map.fromFoldable labels1

marking1 :: Marking
marking1 = Bag.fromFoldable
  [ 1 /\ 1
  , 4 /\ 1
  ]

placePoints1 :: Array (Int /\ Vec2D)
placePoints1 =
  [ 1 /\ { y: 30.0, x: 10.0 }
  , 2 /\ { y: 30.0, x: 30.0 }
  , 3 /\ { y: 30.0, x: 90.0 }
  , 4 /\ { y: 30.0, x: 70.0 }
  , 5 /\ { y: 30.0, x: 50.0 }
  ]

placePointsDict1 = Map.fromFoldable placePoints1

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

leBase_TODO = 100
transitionPointsDict1 = Map.fromFoldable $ zipWithIndexFrom leBase_TODO transitionPoints1

transitionsDict1 :: Map Int Transition
transitionsDict1 = Map.fromFoldable $ zipWithIndexFrom leBase_TODO transitions1

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Tuple Int v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> Tuple (i0+i) x) xs

transitionPoints1 =
  [ { x: 30.0, y: 20.0 }
  , { x: 30.0, y: 40.0 }
  , { x: 70.0, y: 20.0 }
  , { x: 70.0, y: 40.0 }
  ]

net1Data =
  { places1:               places1
  , transitionsDict1:      transitionsDict1
  , marking:               marking1
  , labelsDict1:           labelsDict1
  , placePointsDict1:      placePointsDict1
  , transitionPointsDict1: transitionPointsDict1
  }

-- TODO eliminate; this is a union of net1Data and some extra API-like methods
net1 :: NetRep
net1 =
  { places               : net1Data.places1
  , transitionsDict      : net1Data.transitionsDict1
  , marking              : net1Data.marking
  , transitionPointsDict : transitionPointsDict1
  , labelsDict           : labelsDict1
  , placePointsDict      : placePointsDict1

  -- API, sort of
  , findTransition       : flip Map.lookup net1Data.transitionsDict1
  , findPlaceLabel       : flip Map.lookup net1Data.labelsDict1

  -- rendering related
  , findPlacePoint       : flip Map.lookup net1Data.placePointsDict1
  , findTransitionPoint  : flip Map.lookup net1Data.transitionPointsDict1
  }

netApi1 :: NetApiF PID TID Tokens
netApi1 =
  { findTokens           : findTokens' net1Data.marking
  }
