module Test.Language.Statebox.Wiring where

import Prelude
import Data.Bifunctor (lmap)
import Data.List as List
import Data.List (List(..))
import Data.String (trim)
import Language.Statebox as Statebox
import Language.Statebox.Hypergraph (HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (Label, stripSpan)
import Language.Statebox.Wiring.Generator.Diagram (toDiagramWithName)
import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)

import Debug.Trace (spy)

spec :: Spec Unit
spec = do
  describe "Statebox wiring compiler" do
    it "should parse wirings correctly" do
      let ast = Statebox.parseWiring wiring1src
      let astDump = spy "ast" $ show $ ast
      let diagram1 = spy "diagram1" $ toDiagramWithName "dummy" <$> ast
      (ast # map (map (lmap stripSpan))) `shouldEqual` pure wiring1expected

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
