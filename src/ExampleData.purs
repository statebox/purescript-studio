module ExampleData
  ( PID
  , TID
  , Tokens
  , NetObj
  , NetApi
  , net1
  , netApi1
  , isTransitionEnabled0
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

-- types specialised to Int index ----------------------------------------------

type PID          = Int
type TID          = Int
type Tokens       = Int
type Transition   = TransitionF   PID Tokens
type Marking      = MarkingF      PID Tokens
type PlaceMarking = PlaceMarkingF PID Tokens

type NetRep = NetRepF PID TID Tokens ()

type NetObj = NetObjF PID TID Tokens

type NetApi = NetApiF PID TID Tokens

--------------------------------------------------------------------------------

mkNetRep
  :: Array PID
  -> Array Transition
  -> Marking
  -> Array (PID /\ String)
  -> Array (PID /\ Vec2D)
  -> Array Vec2D
  -> NetRep
mkNetRep places transitions marking placeLabels placePoints transitionPoints =
  { places:               places
  , transitionsDict:      transitionsDict
  , marking:              marking
  , placeLabelsDict:      placeLabelsDict
  , placePointsDict:      placePointsDict
  , transitionPointsDict: transitionPointsDict
  }
  where
    -- TODO check +1
    firstTransitionIndex = length places + 1

    transitionsDict :: Map Int Transition
    transitionsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitions

    placeLabelsDict :: Map Int String
    placeLabelsDict = Map.fromFoldable placeLabels

    placePointsDict = Map.fromFoldable placePoints

    transitionPointsDict = Map.fromFoldable $ zipWithIndexFrom firstTransitionIndex transitionPoints

-- TODO put this specialised version in this module because it uses `Tokens`, which is defined in here as well
isTransitionEnabled0 :: ∀ pid. Ord pid => MarkingF pid Tokens -> TransitionF pid Tokens -> Boolean
isTransitionEnabled0 = isTransitionEnabled 0

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

transitionPoints1 =
  [ { x: 30.0, y: 20.0 }
  , { x: 30.0, y: 40.0 }
  , { x: 70.0, y: 20.0 }
  , { x: 70.0, y: 40.0 }
  ]

net1Data :: NetRep
net1Data = mkNetRep places1 transitions1 marking1 labels1 placePoints1 transitionPoints1

net1 :: NetObj
net1 = mkNetObjF net1Data

netApi1 :: NetApi
netApi1 =
  { findTokens : findTokens' net1Data.marking
  }

--------------------------------------------------------------------------------

zipWithIndexFrom :: forall v. Int -> Array v -> Array (Tuple Int v)
zipWithIndexFrom i0 xs = mapWithIndex (\i x -> Tuple (i0+i) x) xs
