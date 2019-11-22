module Test.Language.Statebox.Wiring where

import Prelude
import Data.Bifunctor (lmap)
import Data.List as List
import Data.List (List)
import Data.String (trim)
import Language.Statebox as Statebox
import Language.Statebox.Hypergraph (HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (Label, stripSpan)
import Language.Statebox.Wiring.Generator.Diagram (toDiagramWithName)
import Language.Statebox.Wiring.Generator.DiagramV2 (diagramEdges, fromEdges)
import Statebox.Core.Types (Diagram)
import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)

import Debug.Trace (spy)

spec :: Spec Unit
spec = do
  describe "Statebox wiring compiler" do
    it "should parse wirings correctly" do
      let ast = Statebox.parseWiring wiring1src
      let diagram1 = toDiagramWithName "dummy" <$> ast
      let diagramv2 = diagram1 <#> diagramEdges <#> fromEdges <#> \{pixels, context} -> spy pixels (spy context "newline hack")
      (ast # map (map (lmap stripSpan))) `shouldEqual` pure wiring1expected
      diagram1 `shouldEqual` pure diagram1expected

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

diagram1expected :: Diagram
diagram1expected =
  { name: "dummy"
  , width: 5
  , pixels: [ 1, 1, 2, 3, 5, 2, 3, 4, 5, 4 ]
  , names: [ "a1", "b1", "b2", "d1", "c1" ]
  }

--------------------------------------------------------------------------------
-- graph DSL
--------------------------------------------------------------------------------

mkAst gelems = List.fromFoldable gelems

mkEdge ss ts = GHyperEdge (HyperEdge unit (List.fromFoldable ss) (List.fromFoldable ts))
