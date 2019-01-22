module View.Diagram.Update where

import Prelude

import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec2D (Vec3, _x, _y, _z, vec3)
import Web.HTML.HTMLElement (DOMRect)

import View.Diagram.Model
import View.Diagram.Common (snap)

type State =
  { model                   :: Model -- TODO should perhaps be flattened into this record, ie State and Model should be unified
  , msg                     :: String
  , boundingClientRectMaybe :: Maybe DOMRect -- ^ Allows us to correct mouse coordinates for the component's position.
  }

type Query = MkQueryF MouseMsg

data MouseMsg
  = MouseIsOver   Operator OperatorHandle
  | MouseIsOut    Operator
  | MousePosition (Vec3 Int)
  | MouseUp       (Vec3 Int)
  | MouseDown     (Vec3 Int)

-- TODO Coyoneda?
data MkQueryF e a = QueryF e a

--------------------------------------------------------------------------------

evalModel :: MouseMsg -> Model -> Model
evalModel msg model = case msg of
  MouseIsOut    _   -> model { mouseOver = Nothing }
  MouseIsOver   x k -> model { mouseOver = Just (x /\ k) }
  MousePosition p   -> model { mousePosition = p }
  MouseDown     p   -> model { mousePosition = p
                             , mousePressed = true
                             , dragStart = case model.mouseOver of
                                             Nothing                 -> DragStartedOnBackground model.mousePosition
                                             Just (op /\ opPosition) -> DragStartedOnOperator   model.mousePosition op opPosition
                             }
  MouseUp       p   -> (dropGhost model) { mousePosition = p
                                         , mousePressed = false
                                         , dragStart = DragNotStarted
                                         }

--------------------------------------------------------------------------------

dropGhost :: Model -> Model
dropGhost model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let s = model.config.scale
        dxdydw   = dragDelta model
        sdxdydw = map (snap s) dxdydw
        mdxdydw = map (\x -> x/s) sdxdydw
        opxyw   = op.position - mdxdydw
        (cw /\ ch)          = (model.config.width /\ model.config.height)
        isValid             = isPositive && isBounded
        isPositive          = (_x opxyw >= 0)       && (_y opxyw >= 0)
        isBounded           = (_x opxyw < (cw / s)) && (_y opxyw < (ch / s))
        -- TODO ^ add condition for w
        (ox /\ ow) = if _z opxyw > 0 then _x opxyw /\ _z opxyw else (_x opxyw + _z opxyw) /\ (- _z opxyw)
        modOp o = o { position = vec3 ox (_y opxyw) ow }
        newOps = modifyOperator op.identifier modOp model.ops
    in if isValid then model { ops = newOps } else model
  _ -> model
