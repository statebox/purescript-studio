-- | Temporary fake Typedefs for stubbing and demonstration purposes/
module Data.Typedef.Typedef2 where

-- | A dummy Typedef-like type with at least some structure so we can show off
-- | some features of the editor component.
data Typedef2
  = TSum (Array Typedef2)
  | TProd (Array Typedef2)
  | TUnit
  | TRef String

data OperatorKind = Plus | Times | Mu

type TypeName = String
