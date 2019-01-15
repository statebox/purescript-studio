module View.Diagram.Model where

import Prelude
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec2(..), doubleMap)

import View.Diagram.Common (snap, showVec3)

type DiagramInfo = { name :: String }

type Operator =
  { identifier :: String -- must be unique; problematic, want to lenses instead
  , x          :: Int
  , y          :: Int
  , w          :: Int
  , label      :: String
  }

type Config =
  { scale  :: Int
  , width  :: Int
  , height :: Int
  }

data OperatorHandle = OpCenter | OpLeft | OpRight

instance showOperatorHandle :: Show OperatorHandle where
  show = case _ of
    OpLeft   -> "OpLeft"
    OpCenter -> "OpCenter"
    OpRight  -> "OpRight"

matchOperatorHandle :: forall a. OperatorHandle -> a -> a -> a -> a
matchOperatorHandle op l c r = case op of
  OpLeft   -> l
  OpCenter -> c
  OpRight  -> r

data DragStart
  = DragNotStarted
  | DragStartedOnBackground (Vec2 Int)
  | DragStartedOnOperator   (Vec2 Int) Operator OperatorHandle

instance showDragStart :: Show DragStart where
  show = case _ of
    DragNotStarted                  -> "DragNotstarted"
    DragStartedOnBackground xy      -> "DragStartedOnBackground " <> show xy
    DragStartedOnOperator   xy o oh -> "DragStartedOnOperator "   <> show xy  <> " " <> show o <> " " <> show oh

type Model =
  { ops           :: Array Operator
    -- this String id is somewhat problematic; it's an id into a "Array/Set" of operators; rather have a Lens here
  , mouseOver     :: Maybe (Operator /\ OperatorHandle)
  , mousePosition :: Vec2 Int
  , mousePressed  :: Boolean
  , dragStart     :: DragStart
  , config        :: Config
  }

modifyOperator :: String -> (Operator -> Operator) -> Array Operator -> Array Operator
modifyOperator s f =
  map modify
  where
    modify o = if o.identifier == s then f o else o

dragDelta :: Model -> (Vec2 Int /\ Int)
dragDelta model = case model.dragStart of
  DragStartedOnBackground pt ->
    let dxdy = sub pt model.mousePosition
    in dxdy /\ 0
  DragStartedOnOperator pt _ handle ->
    let
      dxdy = sub pt model.mousePosition
    in
      dragDeltaOperator dxdy handle
  DragNotStarted -> (zero /\ 0)

dragDeltaOperator :: Vec2 Int -> OperatorHandle -> Vec2 Int /\ Int
dragDeltaOperator v@(Vec2 {x: x, y: y}) handle =
  matchOperatorHandle handle
    (v                   /\ -x)
    (v                   /\ 0 )
    ((Vec2 {x: 0, y: y}) /\ x )

isValidDrag :: Model -> Boolean
isValidDrag model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let s = model.config.scale
        -- TODO add condition where dw is involved
        (dxdy /\ _) = dragDelta model
        sdxdy       = doubleMap (snap s) (snap s)
        (mdx /\ mdy)    = (sdx / s)    /\ (sdy / s)
        (opX /\ opY)    = (op.x - mdx) /\ (op.y - mdy)
    --     (cw, ch) = (model.config.width, model.config.height)
    --     isPositive = (opX > 0) && (opY > 0)
    --     isBounded = (opX < (cw - s)) && (opY < (ch - s))
    --     isValid = isPositive && isBounded
    in (opX > 0) && (opY > 0)
  _ -> false
