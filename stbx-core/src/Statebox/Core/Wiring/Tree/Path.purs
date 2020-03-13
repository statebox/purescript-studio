module Statebox.Core.Wiring.Tree where

-- | A path to a leaf (i.e. a net) in the diagram tree.
type Path = Array PathElem

type PathElem = Int
