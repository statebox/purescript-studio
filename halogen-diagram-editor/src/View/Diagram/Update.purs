module View.Diagram.Update where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec3 (Vec3, _x, _y, _z, vec3)
import Web.HTML (HTMLElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import View.Diagram.Model
import View.Diagram.Common (snap)

type State =
  { model              :: Model -- TODO should perhaps be flattened into this record, ie State and Model should be unified
  , msg                :: String
  , keyHelpVisible     :: Boolean
  , componentElemMaybe :: Maybe HTMLElement
  , inspectorVisible   :: Boolean
  }

data Action
  = MouseAction MouseMsg
  | MoveCursor (Vec3 Int)
  | CreateOp
  | ToggleKeyHelp
  | ToggleInspector

data MouseMsg
  = MouseIsOver Operator OperatorHandle
  | MouseIsOut  Operator
  | MousePos    (Vec3 Int)
  | MouseUp     (Vec3 Int)
  | MouseDown   (Vec3 Int)

data Msg
  = OperatorClicked OperatorId
  | OperatorsChanged (Array Operator)

--------------------------------------------------------------------------------

-- | The `Maybe Operators` in the result indicates if any ops were modified.
evalModel :: MouseMsg -> Operators -> Model -> Maybe Operators /\ Model
evalModel msg ops model = case msg of
  MouseIsOut    _   -> Nothing     /\ model { mouseOver = Nothing }
  MouseIsOver   x k -> Nothing     /\ model { mouseOver = Just (x /\ k) }
  MousePos      p   -> Nothing     /\ model { mousePos = p }
  MouseDown     p   -> Nothing     /\ model { mousePos = p
                                            , mousePressed = true
                                            , dragStart = case model.mouseOver of
                                                            Nothing            -> DragStartedOnBackground model.mousePos
                                                            Just (op /\ opPos) -> DragStartedOnOperator   model.mousePos op opPos
                                            }
  MouseUp       p   -> opsModified /\ model { mousePos = p
                                            , mousePressed = false
                                            , dragStart = DragNotStarted
                                            }
    where
      opsModified = dropGhost ops model

--------------------------------------------------------------------------------

-- | The `Maybe Operators` in the result indicates if any ops were modified.
dropGhost :: Operators -> Model -> Maybe Operators
dropGhost ops model = case model.dragStart of
  DragStartedOnOperator _ op _ -> if isValid then Just newOps else Nothing
    where
      scale      = model.config.scale
      dd         = dragDelta model
      ddScreen   = snap scale <$> dd
      ddModel    = (_/scale) <$> ddScreen
      opxyw      = op.pos - ddModel
      (cw /\ ch) = model.config.width /\ model.config.height
      isValid    = isPositive && isBounded
      isPositive = (_x opxyw >= zero)        && (_y opxyw >= zero)
      isBounded  = (_x opxyw < (cw / scale)) && (_y opxyw < (ch / scale))
      -- TODO ^ add condition for w
      (ox /\ ow) = if _z opxyw > zero then _x opxyw /\ _z opxyw else (_x opxyw + _z opxyw) /\ (- _z opxyw)
      modOp o    = o { pos = vec3 ox (_y opxyw) ow }
      newOps     = modifyOperator op.identifier modOp ops
  _ -> Nothing
