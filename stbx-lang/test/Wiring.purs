module Test.Language.Statebox.Wiring where

import Prelude
import Data.List as List
import Data.List (List(..))
import Data.String (trim)
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Statebox as Statebox
import Language.Statebox.Hypergraph (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (Node(..), GElem(..), HyperEdge(..), Label)
import Test.Spec                  (Spec, describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

spec :: Spec _
spec = do
  describe "Statebox wiring compiler" do
    pending "Implement wiring tests."

wiring1src :: String
wiring1src = trim """
a1 -> b1, b2
b1 -> d1
b2 -> c1
c1 -> d1
"""

wiring1expected :: List (GElemF List Label Unit)
wiring1expected = mkAst
  [ mkEdge ["a1"] ["b1", "b2"]
  , mkEdge ["b1"] ["d1"]
  , mkEdge ["b2"] ["c1"]
  , mkEdge ["c1"] ["d1"]
  ]


--------------------------------------------------------------------------------
-- graph DSL
--------------------------------------------------------------------------------

mkAst gelems = List.fromFoldable gelems

mkEdge ss ts = GHyperEdge (HyperEdge unit (List.fromFoldable ss) (List.fromFoldable ts))
