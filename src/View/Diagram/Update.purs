module View.Diagram.Update where

import Prelude

import Data.Maybe
import Data.Tuple.Nested ((/\))
import Data.Vec3 (Vec3, _x, _y, _z, vec3)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (DOMRect)

import View.Diagram.Model
import View.Diagram.Common (snap)

type State =
  { model                   :: Model -- TODO should perhaps be flattened into this record, ie State and Model should be unified
  , msg                     :: String
  , htmlElementMaybe        :: Maybe HTMLElement
  }

data Query a
  = Initialize a
  | MouseAction MouseMsg a
  | UpdateDiagram Operators a

data MouseMsg
  = MouseIsOver Operator OperatorHandle
  | MouseIsOut  Operator
  | MousePos    (Vec3 Int)
  | MouseUp     (Vec3 Int)
  | MouseDown   (Vec3 Int)

data Msg =
  OperatorClicked OperatorId

--------------------------------------------------------------------------------

evalModel :: MouseMsg -> Model -> Model
evalModel msg model = case msg of
  MouseIsOut    _   -> model { mouseOver = Nothing }
  MouseIsOver   x k -> model { mouseOver = Just (x /\ k) }
  MousePos      p   -> model { mousePos = p }
  MouseDown     p   -> model { mousePos = p
                             , mousePressed = true
                             , dragStart = case model.mouseOver of
                                             Nothing            -> DragStartedOnBackground model.mousePos
                                             Just (op /\ opPos) -> DragStartedOnOperator   model.mousePos op opPos
                             }
  MouseUp       p   -> (dropGhost model) { mousePos = p
                                         , mousePressed = false
                                         , dragStart = DragNotStarted
                                         }

--------------------------------------------------------------------------------

dropGhost :: Model -> Model
dropGhost model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let scale      = model.config.scale
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
        newOps     = modifyOperator op.identifier modOp model.ops
    in if isValid then model { ops = newOps } else model
  _ -> model
