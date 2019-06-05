module Statebox.Core.Execution where

import Prelude
import Data.Array (index, length, (..))
import Data.ArrayMultiset (ArrayMultiset)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Statebox.Core.Types (PID, TID, Wiring, GluedTransitionId(..))

import Unsafe.Coerce (unsafeCoerce)


-- | ReasonML-encoded representation of a marked net.
foreign import data Marked :: Type

foreign import data StbxObj :: Type

-- TODO make 'sort' a type alias/newtype instead of String?
type GluedTransition1 =
  { sort   :: String -- ^ see `TransitionSort`.
  , tset   :: TSet
  }

-- TODO Statebox.Types.Firing has a field 'path', which should be called 'firing', says Jelle
foreign import data GluedTransition2JS :: Type

fromGluedTransition2JS :: GluedTransition2JS -> GluedTransition2
fromGluedTransition2JS = unsafeCoerce

-- TODO 1) Should (Array (Array Int)) be something like (Array (Tuple TID))?
-- TODO 2) Do these field names make any sense?
-- | Map transitions in the glued net back onto their constituent nets and transitions.
type GluedTransition2 =
  { pre  :: Array (Array TID)
  , post :: String
  }

-- | When gluing two nets together, the resulting transitions are classified as follows:
-- | ```
-- |      _     _     _        _     _
-- |     |_|-O-|_|-O-|_|      |_|-O-|_|
-- |
-- |      |     |     \________/     |
-- |      |     |          |         |
-- |  Initial  Normal    Glued     Final
-- |      |     |          |         |
-- |      V     V          V         V
-- |      _     _          _         _
-- |     |_|-O-|_|-O------|_|-----O-|_|
-- | ```
data TransitionSort
  = Initial
  | Normal
  | Glued
  | Final

fromTransitionSortString :: String -> Maybe TransitionSort
fromTransitionSortString = case _ of
  "initial" -> Just Initial
  "glued"   -> Just Glued
  "final"   -> Just Final
  _         -> Nothing

derive instance eqTransitionSort :: Eq TransitionSort

instance showTransitionSort :: Show TransitionSort where
  show = case _ of
    Initial -> "initial"
    Normal  -> "normal"
    Glued   -> "glued"
    Final   -> "final"

type PathElem = Int

-- | A path to a leaf (i.e. a net) in the diagram tree.
type Path = Array PathElem

-- TODO Since we already have Path as an isolated type, this PathIndexed may not add much value
type PathIndexed r = (path :: Path | r)

type TSet = Record (PathIndexed (transition :: TID))

type GluedMarking = Array MarkingRec

type MarkingRec = Record (PathIndexed (marking :: ArrayMultiset PID))

-- | Constructor.
foreign import fromWiring       :: Wiring -> StbxObj

-- | Constructor.
foreign import fromMarked       :: Marked -> StbxObj

foreign import marking          :: StbxObj -> GluedMarking

-- Since this isn't orthogonal, do we really want it? We can get this from (length .transitions())
foreign import transitionCount  :: StbxObj -> Int

foreign import placeCount       :: StbxObj -> Int

--------------------------------------------------------------------------------

-- TODO StbxObj and Marked are distinguished here, which makes things seem less simple and chainable than they are.
foreign import fireOrDefault :: forall a. (Unit -> a) -> (Marked -> a) -> StbxObj -> TID -> a

fire :: StbxObj -> TID -> Maybe Marked
fire = fireOrDefault (const Nothing) Just

--------------------------------------------------------------------------------

-- TODO Do we want to provide a flipped version of 'enabled'?

-- TODO TID or GluedTransitionId?

enabledMaybe :: StbxObj -> TID -> Maybe Boolean
enabledMaybe = enabledOrDefault (const Nothing) Just

enabled :: StbxObj -> TID -> Boolean
enabled = enabledOrDefault (const false) identity

-- TODO eliminate; should take either a GluedTransitionId *or* TID, but not both
enabledMaybe_glued :: StbxObj -> GluedTransitionId -> Maybe Boolean
enabledMaybe_glued s (GluedTransitionId i) = enabledMaybe s i

-- enabled_glued :: StbxObj -> TID -> Boolean
-- enabled_glued = enabledOrDefault (const false) identity

foreign import enabledOrDefault :: forall a. (Unit -> a) -> (Boolean -> a) -> StbxObj -> TID -> a

--------------------------------------------------------------------------------

foreign import gluedTransitions :: StbxObj -> Array GluedTransition1

-- als we dit hebben dan willen we wellicht de fn 'transitions' hiden? want die geeft een array terug
-- TODO is it inefficient not to cache these?
getGluedTransition :: StbxObj -> GluedTransitionId -> Maybe GluedTransition1
getGluedTransition s i = gluedTransitions s `index` unwrap i

--------------------------------------------------------------------------------

-- TODO add 'glued' to name?
transitionIdsUpToAndIncluding :: GluedTransitionId -> Array GluedTransitionId
transitionIdsUpToAndIncluding (GluedTransitionId i) = GluedTransitionId <$> 0 .. i

-- TODO add 'glued' to name?
transitionIds :: StbxObj -> Array GluedTransitionId
transitionIds s = transitionIdsUpToAndIncluding (GluedTransitionId $ (transitionCount s) - 1)

--------------------------------------------------------------------------------

-- What is called 'firing' here is really static info about a glued transition.

-- TODO Is it inefficient not to cache these?
getFiring :: StbxObj -> GluedTransitionId -> Maybe GluedTransition2JS
getFiring s gti@(GluedTransitionId i) =
  if i < transitionCount s then Just (firing s gti) else Nothing

-- TODO unsafe; see getFiring
foreign import firing :: StbxObj -> GluedTransitionId -> GluedTransition2JS
