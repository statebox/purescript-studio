module View.Diagram.Model where

import Prelude
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))

import View.Diagram.Common (snap, dxdy, showVec2, showVec3)

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
  | DragStartedOnBackground (Int /\ Int)
  | DragStartedOnOperator   (Int /\ Int) Operator OperatorHandle

instance showDragStart :: Show DragStart where
  show = case _ of
    DragNotStarted                  -> "DragNotstarted"
    DragStartedOnBackground xy      -> "DragStartedOnBackground " <> showVec2 xy
    DragStartedOnOperator   xy o oh -> "DragStartedOnOperator "   <> showVec2 xy  <> " " <> show o <> " " <> show oh

type Model =
  { ops           :: Array Operator
    -- this String id is somewhat problematic; it's an id into a "Array/Set" of operators; rather have a Lens here
  , mouseOver     :: Maybe (Operator /\ OperatorHandle)
  , mousePosition :: (Int /\ Int)
  , mousePressed  :: Boolean
  , dragStart     :: DragStart
  , config        :: Config
  }

modifyOperator :: String -> (Operator -> Operator) -> Array Operator -> Array Operator
modifyOperator s f =
  map modify
  where
    modify o = if o.identifier == s then f o else o

dragDelta :: Model -> (Int /\ Int /\ Int)
dragDelta model = case model.dragStart of
  DragStartedOnBackground pt ->
    let (dx /\ dy) = dxdy pt model.mousePosition
    in (dx /\ dy /\ 0)
  DragStartedOnOperator pt _ handle ->
    let
      (dx /\ dy) = dxdy pt model.mousePosition
      fdx = matchOperatorHandle handle    dx dx  0
      gdx = matchOperatorHandle handle (-dx)  0 dx
    in
      (fdx /\ dy /\ gdx)
  DragNotStarted -> (0 /\ 0 /\ 0)

isValidDrag :: Model -> Boolean
isValidDrag model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let s = model.config.scale
        -- TODO add condition where dw is involved
        (dx /\ dy /\ _) = dragDelta model
        (sdx /\ sdy)    = snap s dx    /\ snap s dy
        (mdx /\ mdy)    = (sdx / s)    /\ (sdy / s)
        (opX /\ opY)    = (op.x - mdx) /\ (op.y - mdy)
    --     (cw, ch) = (model.config.width, model.config.height)
    --     isPositive = (opX > 0) && (opY > 0)
    --     isBounded = (opX < (cw - s)) && (opY < (ch - s))
    --     isValid = isPositive && isBounded
    in (opX > 0) && (opY > 0)
  _ -> false
