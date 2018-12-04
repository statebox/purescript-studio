module DiagramEditor.Update where

import Prelude

import Data.Maybe
import Data.Tuple.Nested (type (/\), (/\))
import Web.HTML.HTMLElement (DOMRect)

import DiagramEditor.Model
import DiagramEditor.Common (snap)

type State =
  { model                   :: Model -- TODO should perhaps be flattened into this record, ie State and Model should be unified
  , msg                     :: String
  , boundingClientRectMaybe :: Maybe DOMRect -- ^ Allows us to correct mouse coordinates for the component's position.
  }

type Query = MkQueryF MouseMsg

data MouseMsg
  = MouseIsOver   Operator OperatorHandle
  | MouseIsOut    Operator
  | MousePosition Int Int
  | MouseUp       Int Int
  | MouseDown     Int Int

-- TODO Coyoneda?
data MkQueryF e a = QueryF e a

--------------------------------------------------------------------------------

evalModel :: MouseMsg -> Model -> Model
evalModel msg model = case msg of
  MouseIsOut    _   -> model { mouseOver = Nothing }
  MouseIsOver   x k -> model { mouseOver = Just (x /\ k) }
  MousePosition x y -> model { mousePosition = x /\ y }
  MouseDown     x y -> model { mousePosition = x /\ y
                             , mousePressed = true
                             , dragStart = case model.mouseOver of
                                             Nothing                 -> DragStartedOnBackground model.mousePosition
                                             Just (op /\ opPosition) -> DragStartedOnOperator   model.mousePosition op opPosition
                             }
  MouseUp       x y -> (dropGhost model) { mousePosition = x /\ y
                                         , mousePressed = false
                                         , dragStart = DragNotStarted
                                         }

--------------------------------------------------------------------------------

dropGhost :: Model -> Model
dropGhost model = case model.dragStart of
  DragStartedOnOperator _ op _ ->
    let s = model.config.scale
        (dx  /\ dy  /\ dw) = dragDelta model
        (sdx /\ sdy /\ sdw) = (snap s dx /\ snap s dy /\ snap s dw)
        (mdx /\ mdy /\ mdw) = (sdx / s /\ sdy / s /\ sdw / s)
        (opX /\ opY /\ opW) = (op.x - mdx) /\ (op.y - mdy) /\ (op.w - mdw)
        (cw /\ ch)          = (model.config.width /\ model.config.height)
        isValid             = isPositive && isBounded
        isPositive          = (opX >= 0)       && (opY >= 0)
        isBounded           = (opX < (cw / s)) && (opY < (ch / s))
        -- TODO ^ add condition for w
        (ox /\ ow) = if opW > 0 then opX /\ opW else (opX + opW) /\ (-opW)
        modOp o = o { x = ox, y = opY, w = ow }
        newOps = modifyOperator op.identifier modOp model.ops
    in if isValid then model { ops = newOps } else model
  _ -> model
