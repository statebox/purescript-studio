module View.Diagram.Model where

import Prelude
import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (Vec3, vec3, _x, _y)

import View.Diagram.Common (snap)

type DiagramInfo =
  { name :: String
  , ops  :: Array Operator
  }

-- must be unique; problematic, want to lenses instead
type OperatorId = String

type Operator =
  { identifier :: OperatorId
  , pos        :: Vec3 Int
  , label      :: String
  }

type Operators = Array Operator

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
  , selectedOpId  :: Maybe OperatorId
    -- this String id is somewhat problematic; it's an id into a "Array/Set" of operators; rather have a Lens here
  , mouseOver     :: Maybe (Operator /\ OperatorHandle)
  , mousePos      :: Vec3 Int
  , mousePressed  :: Boolean
  , dragStart     :: DragStart
  , config        :: Config
  }

modifyOperator :: String -> (Operator -> Operator) -> Array Operator -> Array Operator
modifyOperator ident f ops = fMatched <$> ops
  where
    fMatched op = if op.identifier == ident then f op else op

dragDelta :: Model -> Vec3 Int
dragDelta model = case model.dragStart of
  DragStartedOnBackground pt          -> pt - model.mousePos
  DragStartedOnOperator   pt _ handle -> dragDeltaOperator (pt - model.mousePos) handle
  DragNotStarted                      -> zero

dragDeltaOperator :: Vec3 Int -> OperatorHandle -> Vec3 Int
dragDeltaOperator v handle =
  matchOperatorHandle handle
    (vec3 (_x v) (_y v) (-_x v))
    (vec3 (_x v) (_y v) zero   )
    (vec3 zero   (_y v) (_x v) )

isValidDrag :: Model -> Boolean
isValidDrag model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let scale = model.config.scale
        dd       = dragDelta model
        ddScreen  = vec3 (snap scale) (snap scale) identity <*> dd
        ddModel   = vec3 (_/ scale)   (_ / scale)  identity <*> ddScreen
        opPos     = (vec3 identity identity zero <*> op.pos) - ddModel
    --  -- TODO add condition where dw is involved
    --  (cw, ch) = (model.config.width, model.config.height)
    --  isPositive = (opX > 0) && (opY > 0)
    --  isBounded = (opX < (cw - scale)) && (opY < (ch - scale))
    --  isValid = isPositive && isBounded
    in _x opPos > 0 && _y opPos > 0
  _ -> false
