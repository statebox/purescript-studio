module View.Diagram.Model where

import Prelude
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec3, vec3, _x, _y)

import View.Diagram.Common (snap)

type DiagramInfo = { name :: String }

type Operator =
  { identifier :: String -- must be unique; problematic, want to lenses instead
  , position   :: Vec3 Int
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
  | DragStartedOnBackground (Vec3 Int)
  | DragStartedOnOperator   (Vec3 Int) Operator OperatorHandle

instance showDragStart :: Show DragStart where
  show = case _ of
    DragNotStarted                  -> "DragNotstarted"
    DragStartedOnBackground xy      -> "DragStartedOnBackground " <> show xy
    DragStartedOnOperator   xy o oh -> "DragStartedOnOperator "   <> show xy  <> " " <> show o <> " " <> show oh

type Model =
  { ops           :: Array Operator
    -- this String id is somewhat problematic; it's an id into a "Array/Set" of operators; rather have a Lens here
  , mouseOver     :: Maybe (Operator /\ OperatorHandle)
  , mousePosition :: Vec3 Int
  , mousePressed  :: Boolean
  , dragStart     :: DragStart
  , config        :: Config
  }

modifyOperator :: String -> (Operator -> Operator) -> Array Operator -> Array Operator
modifyOperator s f =
  map modify
  where
    modify o = if o.identifier == s then f o else o

dragDelta :: Model -> Vec3 Int
dragDelta model = case model.dragStart of
  DragStartedOnBackground pt ->
    let dxdy = sub pt model.mousePosition
    in dxdy
  DragStartedOnOperator pt _ handle ->
    let
      dxdy = sub pt model.mousePosition
    in
      dragDeltaOperator dxdy handle
  DragNotStarted -> zero

dragDeltaOperator :: Vec3 Int -> OperatorHandle -> Vec3 Int
dragDeltaOperator v handle =
  matchOperatorHandle handle
    (vec3 (_x v) (_y v) (-_x v))
    (vec3 (_x v) (_y v) 0      )
    (vec3 0      (_y v) (_x v) )

isValidDrag :: Model -> Boolean
isValidDrag model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let s = model.config.scale
        -- TODO add condition where dw is involved
        dxdy        = dragDelta model
        sdxdy       = apply (vec3 (snap s)    (snap s)    identity) dxdy
        mdxdy       = apply (vec3 (\x -> x/s) (\x -> x/s) identity) sdxdy
        opxy        = (apply (vec3 identity identity (const 0)) op.position) - mdxdy
    --     (cw, ch) = (model.config.width, model.config.height)
    --     isPositive = (opX > 0) && (opY > 0)
    --     isBounded = (opX < (cw - s)) && (opY < (ch - s))
    --     isValid = isPositive && isBounded
    in _x opxy > 0 && _y opxy > 0
  _ -> false
